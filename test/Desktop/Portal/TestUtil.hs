module Desktop.Portal.TestUtil
  ( successResponse,
    toVariantMap,
    toVariantText,
    TestClient,
    withTestBus,
    withMethodResponse,
    savingRequestArguments,
  )
where

import Control.Concurrent (newEmptyMVar, tryPutMVar, tryReadMVar)
import Control.Exception (finally)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import DBus (BusName, InterfaceName, IsVariant (fromVariant), MemberName, MethodCall (..), ObjectPath, Variant, formatBusName, objectPath_, toVariant)
import DBus.Client (Client, Interface (..), Reply (..), RequestNameReply (..), connectSession, defaultInterface, disconnect, emit, export, makeMethod, nameDoNotQueue, requestName, unexport)
import DBus.Internal.Message (Signal (..))
import DBus.Internal.Types (Atom (AtomText), Signature (..), Value (ValueMap), Variant (Variant))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import GHC.IO.Handle (hGetLine)
import System.Environment (lookupEnv, setEnv)
import System.Process (StdStream (..), createProcess, proc, std_out, terminateProcess)

newtype TestClient = TestClient Client

withTestBus :: (TestClient -> IO ()) -> IO ()
withTestBus cmd = do
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
  flip finally (teardown oldSessionAddr ph) $ do
    addrLine <- hGetLine hOut
    setEnv sessionAddressEnv addrLine
    client <- connectSession
    flip finally (disconnect client) $ do
      requestName client portalBusName [nameDoNotQueue] >>= \case
        NamePrimaryOwner -> cmd (TestClient client)
        reply -> fail ("Can't get portal name: " <> show reply)
  where
    teardown oldSessionAddr ph = do
      terminateProcess ph
      maybe (pure ()) (setEnv sessionAddressEnv) oldSessionAddr

withMethodResponse :: TestClient -> InterfaceName -> MemberName -> [Variant] -> IO () -> IO ()
withMethodResponse (TestClient client) interfaceName methodName methodResponse cmd = do
  export
    client
    portalObjectPath
    defaultInterface
      { interfaceName,
        interfaceMethods = [makeMethod methodName (Signature []) (Signature []) handleMethodCall]
      }
  cmd
  unexport client portalObjectPath
  where
    handleMethodCall methodCall = do
      emitResponseSignal client methodCall methodResponse
      pure (ReplyReturn [toVariant (methodRequestHandle methodCall)])

savingRequestArguments :: TestClient -> InterfaceName -> MemberName -> IO () -> IO [Variant]
savingRequestArguments (TestClient client) interfaceName methodName cmd = do
  argsVar <- newEmptyMVar
  export
    client
    portalObjectPath
    defaultInterface
      { interfaceName,
        interfaceMethods = [makeMethod methodName (Signature []) (Signature []) (handleMethodCall argsVar)]
      }
  cmd
  unexport client portalObjectPath
  tryReadMVar argsVar >>= \case
    Just args -> pure (removeHandleToken args)
    Nothing -> fail "No method was called during the callback!"
  where
    handleMethodCall argsVar methodCall = do
      emitResponseSignal client methodCall [toVariant (1 :: Word32)]
      putSucceeded <- liftIO $ tryPutMVar argsVar (methodCallBody methodCall)
      unless putSucceeded $
        fail "Method arguments already saved: is more than one method being called?"
      pure (ReplyReturn [toVariant (methodRequestHandle methodCall)])

    removeHandleToken = \case
      args
        | (not (null args)),
          Variant (ValueMap kt vt argsMap) <- args !! (length args - 1) ->
            take (length args - 1) args <> [Variant (ValueMap kt vt (Map.delete (AtomText "handle_token") argsMap))]
        | otherwise ->
            args

emitResponseSignal :: MonadIO m => Client -> MethodCall -> [Variant] -> m ()
emitResponseSignal client methodCall signalBody = do
  void . liftIO $
    emit
      client
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
