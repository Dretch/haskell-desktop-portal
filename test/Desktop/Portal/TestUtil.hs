module Desktop.Portal.TestUtil
  ( successResponse,
    toVariantMap,
    toVariantText,
    TestHandle,
    client,
    withTestBus,
    withTestBus_,
    withMethodResponse,
    withMethodResponse_,
    withRequestResponse,
    savingMethodArguments,
    savingMethodArguments_,
    savingRequestArguments,
    sendSignal,
    dbusClientException,
    withTempFilePath,
    withTempFilePaths,
    withTempFd,
    withTempFds,
    withTempDirectoryFd,
    withTempDirectoryFilePath,
  )
where

import Control.Concurrent (newEmptyMVar, tryPutMVar, tryReadMVar)
import Control.Exception (bracket, finally, throwIO)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import DBus (BusName, InterfaceName, IsVariant (fromVariant), MemberName, MethodCall (..), ObjectPath, Variant, formatBusName, getSessionAddress, objectPath_, toVariant)
import DBus.Client (Client, ClientError, ClientOptions (..), Interface (..), Reply (..), RequestNameReply (..), clientError, connectWith, defaultClientOptions, defaultInterface, disconnect, emit, export, makeMethod, nameDoNotQueue, requestName, unexport)
import DBus.Internal.Message (Signal (..))
import DBus.Internal.Types (Atom (AtomText), Signature (..), Value (ValueMap), Variant (Variant))
import DBus.Socket (SocketOptions (..), authenticatorWithUnixFds, defaultSocketOptions)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Desktop.Portal qualified as Portal
import GHC.IO.Handle (hGetLine)
import System.Environment (lookupEnv, setEnv)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Posix (Fd, OpenMode (..), closeFd, defaultFileFlags, handleToFd, openFd)
import System.Process (StdStream (..), createProcess, proc, std_out, terminateProcess)
import Test.Hspec.Expectations (Selector)

data TestHandle = TestHandle
  { serverClient :: Client,
    clientClient :: Portal.Client
  }

client :: TestHandle -> Portal.Client
client c = c.clientClient

withTestBus :: (TestHandle -> IO ()) -> IO ()
withTestBus = withTestBus_ portalBusName

withTestBus_ :: BusName -> (TestHandle -> IO ()) -> IO ()
withTestBus_ busName cmd = do
  let dbusArgs =
        [ "--print-address",
          "--nopidfile",
          "--nofork",
          "--syslog-only",
          "--config-file=test/dbus-config.xml"
        ]
  (_, Just hOut, _, ph) <-
    createProcess (proc "dbus-daemon" dbusArgs) {std_out = CreatePipe}
  oldSessionAddr <- lookupEnv sessionAddressEnv
  flip finally (stopDbus oldSessionAddr ph) $ do
    addrLine <- hGetLine hOut
    setEnv sessionAddressEnv addrLine
    serverClient <- connectSessionWithFds
    flip finally (disconnect serverClient) $ do
      clientClient <- Portal.connect
      flip finally (Portal.disconnect clientClient) $ do
        requestName serverClient busName [nameDoNotQueue] >>= \case
          NamePrimaryOwner -> cmd TestHandle {serverClient, clientClient}
          reply -> fail ("Can't get portal name: " <> show reply)
  where
    stopDbus oldSessionAddr ph = do
      terminateProcess ph
      maybe (pure ()) (setEnv sessionAddressEnv) oldSessionAddr

withMethodResponse :: TestHandle -> InterfaceName -> MemberName -> [Variant] -> IO () -> IO ()
withMethodResponse handle =
  withMethodResponse_ handle portalObjectPath

withMethodResponse_ :: TestHandle -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> IO () -> IO ()
withMethodResponse_ handle objectPath interfaceName methodName methodResponse cmd = do
  export
    handle.serverClient
    objectPath
    defaultInterface
      { interfaceName,
        interfaceMethods = [makeMethod methodName (Signature []) (Signature []) (const . pure . ReplyReturn $ methodResponse)]
      }
  cmd
  unexport handle.serverClient objectPath

withRequestResponse :: TestHandle -> InterfaceName -> MemberName -> [Variant] -> IO () -> IO ()
withRequestResponse handle interfaceName methodName methodResponse cmd = do
  export
    handle.serverClient
    portalObjectPath
    defaultInterface
      { interfaceName,
        interfaceMethods = [makeMethod methodName (Signature []) (Signature []) handleMethodCall]
      }
  cmd
  unexport handle.serverClient portalObjectPath
  where
    handleMethodCall methodCall = do
      emitResponseSignal handle methodCall methodResponse
      pure (ReplyReturn [toVariant (methodRequestHandle methodCall)])

savingMethodArguments :: TestHandle -> InterfaceName -> MemberName -> [Variant] -> IO () -> IO [Variant]
savingMethodArguments handle =
  savingMethodArguments_ handle portalObjectPath

savingMethodArguments_ :: TestHandle -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> IO () -> IO [Variant]
savingMethodArguments_ handle objectPath interfaceName methodName response cmd = do
  argsVar <- newEmptyMVar
  export
    handle.serverClient
    objectPath
    defaultInterface
      { interfaceName,
        interfaceMethods = [makeMethod methodName (Signature []) (Signature []) (handleMethodCall argsVar)]
      }
  cmd
  unexport handle.serverClient objectPath
  tryReadMVar argsVar >>= \case
    Just args -> pure args
    Nothing -> fail "No method was called during the callback!"
  where
    handleMethodCall argsVar methodCall = do
      putSucceeded <- liftIO $ tryPutMVar argsVar (methodCallBody methodCall)
      unless putSucceeded $
        fail "Method arguments already saved: is more than one method being called?"
      pure (ReplyReturn response)

savingRequestArguments :: TestHandle -> InterfaceName -> MemberName -> IO () -> IO [Variant]
savingRequestArguments handle interfaceName methodName cmd = do
  argsVar <- newEmptyMVar
  export
    handle.serverClient
    portalObjectPath
    defaultInterface
      { interfaceName,
        interfaceMethods = [makeMethod methodName (Signature []) (Signature []) (handleMethodCall argsVar)]
      }
  cmd
  unexport handle.serverClient portalObjectPath
  tryReadMVar argsVar >>= \case
    Just args -> pure (removeHandleToken args)
    Nothing -> fail "No method was called during the callback!"
  where
    handleMethodCall argsVar methodCall = do
      putSucceeded <- liftIO $ tryPutMVar argsVar (methodCallBody methodCall)
      unless putSucceeded $
        fail "Method arguments already saved: is more than one method being called?"
      emitResponseSignal handle methodCall [toVariant (1 :: Word32)]
      pure (ReplyReturn [toVariant (methodRequestHandle methodCall)])

    removeHandleToken = \case
      args
        | not (null args),
          Variant (ValueMap kt vt argsMap) <- args !! (length args - 1) ->
            take (length args - 1) args <> [Variant (ValueMap kt vt (Map.delete (AtomText "handle_token") argsMap))]
        | otherwise ->
            args

sendSignal :: TestHandle -> InterfaceName -> MemberName -> [Variant] -> IO ()
sendSignal handle signalInterface signalMember signalBody =
  emit
    handle.serverClient
    Signal
      { signalPath = "/org/freedesktop/portal/desktop",
        signalInterface,
        signalMember,
        signalSender = Just portalBusName,
        signalDestination = Just (Portal.clientName handle.clientClient),
        signalBody
      }

emitResponseSignal :: MonadIO m => TestHandle -> MethodCall -> [Variant] -> m ()
emitResponseSignal handle methodCall signalBody = do
  void . liftIO $
    emit
      handle.serverClient
      Signal
        { signalPath = methodRequestHandle methodCall,
          signalInterface = "org.freedesktop.portal.Request",
          signalMember = "Response",
          signalSender = Just portalBusName,
          signalDestination = methodCallSender methodCall,
          signalBody
        }

methodRequestHandle :: MethodCall -> ObjectPath
methodRequestHandle methodCall =
  objectPath_ $
    "/org/freedesktop/portal/desktop/request/"
      <> escapeClientName senderClientName
      <> "/"
      <> handleToken
  where
    args = methodCallBody methodCall
    Just (optionsArg :: Map Text Variant) = fromVariant (args !! (length args - 1))
    Just (handleToken :: String) = Map.lookup "handle_token" optionsArg >>= fromVariant
    Just senderClientName = methodCall.methodCallSender
    escapeClientName =
      map (\case '.' -> '_'; c -> c) . drop 1 . formatBusName

sessionAddressEnv :: String
sessionAddressEnv = "DBUS_SESSION_BUS_ADDRESS"

portalObjectPath :: ObjectPath
portalObjectPath = "/org/freedesktop/portal/desktop"

portalBusName :: BusName
portalBusName = "org.freedesktop.portal.Desktop"

successResponse :: [(Text, Variant)] -> [Variant]
successResponse pairs =
  [ toVariant (0 :: Word32), -- success code
    toVariantMap pairs
  ]

toVariantMap :: [(Text, Variant)] -> Variant
toVariantMap = toVariant . Map.fromList

-- | Specialised 'toVariant' to avoid need for type assertions.
toVariantText :: Text -> Variant
toVariantText = toVariant

dbusClientException :: Selector ClientError
dbusClientException = const True

connectSessionWithFds :: IO Client
connectSessionWithFds = do
  env <- getSessionAddress
  case env of
    Nothing -> throwIO (clientError "connectSessionWithFds: session address not found.")
    Just addr -> do
      let socketAuthenticator = authenticatorWithUnixFds
          clientSocketOptions = defaultSocketOptions {socketAuthenticator}
          clientOptions = defaultClientOptions {clientSocketOptions}
      connectWith clientOptions addr

withTempFilePath :: (FilePath -> IO ()) -> IO ()
withTempFilePath cmd =
  withSystemTempFile "haskell-desktop-portal" $ \path _handle -> cmd path

withTempFilePaths :: Int -> ([FilePath] -> IO ()) -> IO ()
withTempFilePaths n cmd = go [] n
  where
    go acc = \case
      i | i <= 0 -> cmd acc
      i -> withTempFilePath (\path -> go (path : acc) (i - 1))

withTempFd :: (Fd -> IO ()) -> IO ()
withTempFd cmd =
  withSystemTempFile "haskell-desktop-portal" $ \_path handle -> do
    bracket (handleToFd handle) closeFd cmd

withTempFds :: Int -> ([Fd] -> IO ()) -> IO ()
withTempFds n cmd = go [] n
  where
    go acc = \case
      i | i <= 0 -> cmd acc
      i -> withTempFd (\fd -> go (fd : acc) (i - 1))

withTempDirectoryFd :: (Fd -> IO ()) -> IO ()
withTempDirectoryFd cmd =
  withSystemTempDirectory "haskell-desktop-portal" $ \path -> do
    bracket (openFd path ReadOnly Nothing defaultFileFlags) closeFd cmd

withTempDirectoryFilePath :: (FilePath -> IO ()) -> IO ()
withTempDirectoryFilePath =
  withSystemTempDirectory "haskell-desktop-portal"
