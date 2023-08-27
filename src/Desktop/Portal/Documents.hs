module Desktop.Portal.Documents
  ( -- * Common Types
    FileIdentifier (..),
    ApplicationId (..),
    DocumentId (..),
    AddFlag (..),
    GrantPermission (..),
    ExtraResults (..),

    -- * Documents Portal Methods
    getMountPoint,
    add,
    addFull,
    addNamed,
    addNamedFull,
    grantPermissions,
    revokePermissions,
    delete,
  )
where

import Control.Exception (bracket, throwIO)
import Control.Monad (void)
import DBus (BusName, InterfaceName, MemberName, ObjectPath, Variant)
import DBus qualified
import DBus.Client qualified as DBus
import Data.Bits (Ior (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString)
import Data.Text (Text, unpack)
import Data.Word (Word32)
import Desktop.Portal.Internal (Client, callMethod_)
import Desktop.Portal.Util (decodeNullTerminatedUtf8, encodeNullTerminatedUtf8)
import System.Posix (Fd)
import System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, openFd)

-- | Specifies a file, either with a file descriptor or a path (which will be resolved to a
-- file descriptor before passing it to the portals API, since the API requires file descriptors).
data FileIdentifier
  = DocumentFilePath FilePath
  | DocumentFd Fd
  deriving (Eq, Show)

newtype ApplicationId = ApplicationId Text
  deriving newtype (Eq, Ord, Show, IsString)

newtype DocumentId = DocumentId Text
  deriving newtype (Eq, Ord, Show, IsString)

data AddFlag
  = AddReuseExisting
  | AddPersistent
  | AddAsNeededByApp
  | AddExportDirectory
  deriving (Eq, Show)

data GrantPermission
  = GrantRead
  | GrantWrite
  | GrantGrantPermissions
  | GrantDelete
  deriving (Eq, Show)

newtype ExtraResults = ExtraResults {mountpoint :: FilePath}
  deriving (Eq, Show)

documentsInterface :: InterfaceName
documentsInterface = "org.freedesktop.portal.Documents"

documentsBusName :: BusName
documentsBusName = "org.freedesktop.portal.Documents"

documentsObject :: ObjectPath
documentsObject = "/org/freedesktop/portal/documents"

getMountPoint :: Client -> IO FilePath
getMountPoint client = do
  callDocumentsMethod client "GetMountPoint" [] >>= \case
    [toFilePath -> Just path] ->
      pure path
    res ->
      throwIO . DBus.clientError $ "getMountPoint: could not parse response: " <> show res

-- | Add a file to the documents store, with basic configuration options.
add ::
  Client ->
  -- | The file to add to the documents store.
  FileIdentifier ->
  -- | Whether to re-use the existing entry in the documents store, if this file is already there.
  Bool ->
  -- | Whether this file should stay in the documents store after this app shuts down.
  Bool ->
  -- | The id (folder name) of the file in the store.
  IO DocumentId
add client file reuseExisting persistent =
  withFd file $ \fd -> do
    callDocumentsMethod client "Add" (args fd) >>= \case
      [DBus.fromVariant -> Just docId] ->
        pure (DocumentId docId)
      res ->
        throwIO . DBus.clientError $ "add: could not parse response: " <> show res
  where
    args fd =
      [ DBus.toVariant fd,
        DBus.toVariant reuseExisting,
        DBus.toVariant persistent
      ]

-- | Add multiple files to the document store, with full configuration options.
addFull ::
  Client ->
  -- | The files to add to the documents store.
  [FileIdentifier] ->
  -- | The flags to apply to the files.
  [AddFlag] ->
  -- | The id of another application that will be granted access to the files.
  Maybe ApplicationId ->
  -- | The permissions to grant to the other application.
  [GrantPermission] ->
  -- | The id (folder name) of each file in the store.
  IO ([DocumentId], ExtraResults)
addFull client files flags appId permissions =
  withFds files $ \fds -> do
    callDocumentsMethod client "AddFull" (args fds) >>= \case
      [DBus.fromVariant -> Just docIds, toExtraResults -> Just extra] ->
        pure (DocumentId <$> docIds, extra)
      res ->
        throwIO . DBus.clientError $ "addFull: could not parse response: " <> show res
  where
    args fds =
      [ DBus.toVariant fds,
        DBus.toVariant (encodeAddFlags flags),
        DBus.toVariant (maybe "" (\(ApplicationId ai) -> ai) appId),
        DBus.toVariant (encodeGrantPermission <$> permissions)
      ]

-- | Add a file to the document store with a specified name, with basic configuration options.
addNamed ::
  Client ->
  -- | The parent directory of the file to add to the documents store.
  FileIdentifier ->
  -- | The basename of the file.
  Text ->
  -- | Whether to re-use the existing entry in the documents store, if this file is already there.
  Bool ->
  -- | Whether this file should stay in the documents store after this app shuts down.
  Bool ->
  -- | The id (folder name) of the file in the store.
  IO DocumentId
addNamed client parentDir basename reuseExisting persistent =
  withFd parentDir $ \fd -> do
    callDocumentsMethod client "AddNamed" (args fd) >>= \case
      [DBus.fromVariant -> Just docId] ->
        pure (DocumentId docId)
      res ->
        throwIO . DBus.clientError $ "addNamed: could not parse response: " <> show res
  where
    args fd =
      [ DBus.toVariant fd,
        DBus.toVariant (encodeNullTerminatedUtf8 basename),
        DBus.toVariant reuseExisting,
        DBus.toVariant persistent
      ]

-- | Add a file to the document store with a specified name, with full configuration options.
addNamedFull ::
  Client ->
  -- | The parent directory of the file to add to the documents store.
  FileIdentifier ->
  -- | The basename of the file.
  Text ->
  -- | The flags to apply to the file.
  [AddFlag] ->
  -- | The id of another application that will be granted access to the file.
  Maybe ApplicationId ->
  -- | The permissions to grant to the other application.
  [GrantPermission] ->
  -- | The id (folder name) of the file in the store.
  IO (DocumentId, ExtraResults)
addNamedFull client parentDir basename flags appId permissions =
  withFd parentDir $ \fd -> do
    callDocumentsMethod client "AddNamedFull" (args fd) >>= \case
      [DBus.fromVariant -> Just docId, toExtraResults -> Just extra] ->
        pure (DocumentId docId, extra)
      res ->
        throwIO . DBus.clientError $ "addNamedFull: could not parse response: " <> show res
  where
    args fd =
      [ DBus.toVariant fd,
        DBus.toVariant (encodeNullTerminatedUtf8 basename),
        DBus.toVariant (encodeAddFlags flags),
        DBus.toVariant (maybe "" (\(ApplicationId ai) -> ai) appId),
        DBus.toVariant (encodeGrantPermission <$> permissions)
      ]

grantPermissions :: Client -> DocumentId -> ApplicationId -> [GrantPermission] -> IO ()
grantPermissions client (DocumentId docId) (ApplicationId appId) permissions =
  void $ callDocumentsMethod client "GrantPermissions" args
  where
    args =
      [ DBus.toVariant docId,
        DBus.toVariant appId,
        DBus.toVariant (encodeGrantPermission <$> permissions)
      ]

revokePermissions :: Client -> DocumentId -> ApplicationId -> [GrantPermission] -> IO ()
revokePermissions client (DocumentId docId) (ApplicationId appId) permissions =
  void $ callDocumentsMethod client "RevokePermissions" args
  where
    args =
      [ DBus.toVariant docId,
        DBus.toVariant appId,
        DBus.toVariant (encodeGrantPermission <$> permissions)
      ]

delete :: Client -> DocumentId -> IO ()
delete client (DocumentId docId) =
  void $ callDocumentsMethod client "Delete" [DBus.toVariant docId]

callDocumentsMethod :: Client -> MemberName -> [Variant] -> IO [Variant]
callDocumentsMethod client =
  callMethod_ client documentsBusName documentsObject documentsInterface

withFd :: FileIdentifier -> (Fd -> IO a) -> IO a
withFd = \case
  DocumentFd fd ->
    ($ fd)
  DocumentFilePath path ->
    bracket (openFd path ReadOnly Nothing defaultFileFlags) closeFd

withFds :: forall a. [FileIdentifier] -> ([Fd] -> IO a) -> IO a
withFds files cmd = withFdsRec [] files
  where
    withFdsRec fds = \case
      [] ->
        cmd (reverse fds)
      file : files' ->
        withFd file $ \fd -> do
          withFdsRec (fd : fds) files'

encodeAddFlags :: [AddFlag] -> Word32
encodeAddFlags flags =
  getIor (foldMap (Ior . encodeAddFlag) flags)

encodeAddFlag :: AddFlag -> Word32
encodeAddFlag = \case
  AddReuseExisting -> 1
  AddPersistent -> 2
  AddAsNeededByApp -> 4
  AddExportDirectory -> 8

encodeGrantPermission :: GrantPermission -> Text
encodeGrantPermission = \case
  GrantRead -> "read"
  GrantWrite -> "write"
  GrantGrantPermissions -> "grant-permissions"
  GrantDelete -> "delete"

toExtraResults :: Variant -> Maybe ExtraResults
toExtraResults v = case DBus.fromVariant v of
  Just (extraMap :: Map Text Variant)
    | Just mountpoint <- toFilePath =<< Map.lookup "mountpoint" extraMap ->
        Just ExtraResults {mountpoint}
  _ ->
    Nothing

toFilePath :: Variant -> Maybe FilePath
toFilePath v =
  unpack <$> (decodeNullTerminatedUtf8 =<< DBus.fromVariant v)
