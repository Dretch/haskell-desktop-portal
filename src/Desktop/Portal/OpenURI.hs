module Desktop.Portal.OpenURI
  ( -- * Open URI
    OpenURIOptions (..),
    openURIOptions,
    openURI,

    -- * Open File
    OpenFileOptions (..),
    openFileOptions,
    openFile,

    -- * Open Directory
    OpenDirectoryOptions (..),
    openDirectoryOptions,
    openDirectory,
  )
where

import DBus (InterfaceName, IsVariant (toVariant))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Desktop.Portal.Internal (Client, FileSpec, Request, sendRequest, withFd)
import Desktop.Portal.Util (toVariantPair)
import Text.URI (URI)
import Text.URI qualified as URI

data OpenURIOptions = OpenURIOptions
  { uri :: URI,
    parentWindow :: Maybe Text,
    writable :: Maybe Bool,
    ask :: Maybe Bool,
    activationToken :: Maybe Text
  }
  deriving (Eq, Show)

data OpenFileOptions = OpenFileOptions
  { fileSpec :: FileSpec,
    parentWindow :: Maybe Text,
    writable :: Maybe Bool,
    ask :: Maybe Bool,
    activationToken :: Maybe Text
  }
  deriving (Eq, Show)

data OpenDirectoryOptions = OpenDirectoryOptions
  { fileSpec :: FileSpec,
    parentWindow :: Maybe Text,
    activationToken :: Maybe Text
  }
  deriving (Eq, Show)

openURIOptions ::
  -- | The URI to open.
  URI ->
  OpenURIOptions
openURIOptions uri =
  OpenURIOptions
    { uri,
      parentWindow = Nothing,
      writable = Nothing,
      ask = Nothing,
      activationToken = Nothing
    }

openFileOptions ::
  -- | The file to open.
  FileSpec ->
  OpenFileOptions
openFileOptions fileSpec =
  OpenFileOptions
    { fileSpec,
      parentWindow = Nothing,
      writable = Nothing,
      ask = Nothing,
      activationToken = Nothing
    }

openDirectoryOptions ::
  -- | The directory to open.
  FileSpec ->
  OpenDirectoryOptions
openDirectoryOptions fileSpec =
  OpenDirectoryOptions
    { fileSpec,
      parentWindow = Nothing,
      activationToken = Nothing
    }

openURIInterface :: InterfaceName
openURIInterface = "org.freedesktop.portal.OpenURI"

openURI :: Client -> OpenURIOptions -> IO (Request ())
openURI client options =
  sendRequest client openURIInterface "OpenURI" args optionsArg parseUnitResponse
  where
    args = [DBus.toVariant parentWindow, DBus.toVariant (URI.render options.uri)]
    parentWindow = fromMaybe "" options.parentWindow
    optionsArg =
      Map.fromList . catMaybes $
        [ toVariantPair "writable" options.writable,
          toVariantPair "ask" options.ask,
          toVariantPair "activation_token" options.activationToken
        ]

openFile :: Client -> OpenFileOptions -> IO (Request ())
openFile client options =
  withFd options.fileSpec $ \fd ->
    sendRequest client openURIInterface "OpenFile" (args fd) optionsArg parseUnitResponse
  where
    args fd = [DBus.toVariant parentWindow, DBus.toVariant fd]
    parentWindow = fromMaybe "" options.parentWindow
    optionsArg =
      Map.fromList . catMaybes $
        [ toVariantPair "writable" options.writable,
          toVariantPair "ask" options.ask,
          toVariantPair "activation_token" options.activationToken
        ]

openDirectory :: Client -> OpenDirectoryOptions -> IO (Request ())
openDirectory client options =
  withFd options.fileSpec $ \fd ->
    sendRequest client openURIInterface "OpenDirectory" (args fd) optionsArg parseUnitResponse
  where
    args fd = [DBus.toVariant parentWindow, DBus.toVariant fd]
    parentWindow = fromMaybe "" options.parentWindow
    optionsArg =
      Map.fromList . catMaybes $
        [toVariantPair "activation_token" options.activationToken]

parseUnitResponse :: a -> IO ()
parseUnitResponse = const (pure ())
