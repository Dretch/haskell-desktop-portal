module Desktop.Portal.Internal
  ( Client,
    connect,
    disconnect,
    clientName,
    Request,
    sendRequest,
    await,
    cancel,
    callMethod,
    callMethod_,
    SignalHandler,
    handleSignal,
    cancelSignalHandler,
  )
where

import Control.Concurrent (MVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (void, when)
import DBus (BusName, InterfaceName, MemberName, MethodCall, ObjectPath)
import DBus qualified
import DBus.Client (ClientError, MatchRule (..))
import DBus.Client qualified as DBus
import DBus.Internal.Message (Signal (..))
import DBus.Internal.Types (Variant)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import Data.Word (Word32, Word64)
import System.Random.Stateful qualified as R

-- | A handle for an active desktop portal session. Can send requests and listen for signals.
data Client = Client
  { dbusClient :: DBus.Client,
    clientName :: BusName
  }

instance Eq Client where
  a == b =
    a.dbusClient.clientThreadID == b.dbusClient.clientThreadID

instance Show Client where
  show c =
    "Client<" <> show c.clientName <> ", " <> show c.dbusClient.clientThreadID <> ">"

-- | A portal request that may be in-progress, finished, or cancelled.
data Request a = Request
  { client :: Client,
    methodCall :: MethodCall,
    signalHandler :: MVar DBus.SignalHandler,
    result :: MVar (Either SomeException (Maybe a))
  }

instance Eq (Request a) where
  a == b = a.result == b.result

instance Show (Request a) where
  show request =
    "Request{client=<"
      <> show request.client
      <> ">, methodCall="
      <> show request.methodCall
      <> ", result=<MVar>}"

-- | A listener for a particular signal. Can be cancelled with 'cancelSignalHandler'.
data SignalHandler = SignalHandler
  { client :: Client,
    dbusSignalHandler :: DBus.SignalHandler
  }

-- | Open a new client connection. This can be used to send requests and listen for signals
-- and finally can be closed using 'disconnect'.
connect :: IO Client
connect = do
  env <- DBus.getSessionAddress
  case env of
    Nothing -> throwIO (DBus.clientError "connect: session address not found.")
    Just addr -> do
      (dbusClient, clientName) <- DBus.connectWithName DBus.defaultClientOptions addr
      pure Client {dbusClient, clientName}

disconnect :: Client -> IO ()
disconnect client = do
  DBus.disconnect client.dbusClient

-- | Get the unique name given to the client by D-BUS.
clientName :: Client -> BusName
clientName = (.clientName)

-- | Wait for a request to be finished, and return the result if it succeeded. If the
-- request is cancelled, either by the user interface or by calling 'cancel', then
-- 'Nothing' will be returned.
await :: Request a -> IO (Maybe a)
await request = do
  readMVar request.result >>= \case
    Left exn -> throwIO exn
    Right res -> pure res

-- | Cancel a request. This will cause any threads blocked on 'await' to receive 'Nothing'.
-- Has no effect if the client is already cancelled or finished successfully.
cancel :: Request a -> IO ()
cancel request = do
  putSucceeded <- tryPutMVar request.result (Right Nothing)
  when putSucceeded $ do
    readMVar request.signalHandler
      >>= DBus.removeMatch request.client.dbusClient

-- | Send a request to the desktop portal D-Bus object and return a handle to the response data.
sendRequest ::
  Client ->
  -- | Which portal interface to invoke.
  InterfaceName ->
  -- | Which method to invoke on that interface.
  MemberName ->
  -- | Positional arguments to pass to the method.
  [Variant] ->
  -- | Named arguments to pass to the method.
  Map Text Variant ->
  -- | A function to parse the method response.
  (Map Text Variant -> IO a) ->
  -- | A handle to the in-progress method call.
  IO (Request a)
sendRequest client interface memberName parameters options parseResponse = do
  (handle, token) <- requestHandle client.clientName

  signalHandlerVar <- newEmptyMVar
  resultVar <- newEmptyMVar

  -- listen before sending the request, to avoid a race condition where the
  -- response happens before we get a chance to register the listener for it
  signalHandler <-
    DBus.addMatch
      client.dbusClient
      DBus.matchAny
        { matchPath = Just handle,
          matchInterface = Just "org.freedesktop.portal.Request",
          matchMember = Just "Response"
        }
      ( \Signal {signalBody} -> do
          val <- case signalBody of
            [code, result]
              | Just (0 :: Word32) <- DBus.fromVariant code,
                Just (resMap :: Map Text Variant) <- DBus.fromVariant result -> do
                  -- catch here: it will be re-thrown in the thread that calls 'await'
                  catch (Right . Just <$> parseResponse resMap) (pure . Left)
            _ -> do
              pure (Right Nothing)
          signalHandler <- readMVar signalHandlerVar
          -- removing match can fail because the client is already disconnected, since this happens
          -- asynchronously, so we have to ignore that (happens all the time during unit tests!)
          catch
            (DBus.removeMatch client.dbusClient signalHandler)
            (\(_ :: ClientError) -> pure ())
          -- need to try because cancel might have been called and populated the mvar with Nothing
          void (tryPutMVar resultVar val)
      )
  putMVar signalHandlerVar signalHandler

  let methodCall =
        (portalMethodCall interface memberName)
          { DBus.methodCallBody =
              parameters <> [DBus.toVariant (Map.insert "handle_token" (DBus.toVariant token) options)]
          }

  reply <- DBus.call_ client.dbusClient methodCall
  case DBus.methodReturnBody reply of
    [x]
      | Just (objX :: ObjectPath) <- DBus.fromVariant x ->
          if objX == handle
            then pure (Request client methodCall signalHandlerVar resultVar)
            else
              let msg = "Unexpected handle: " <> show objX <> " should be " <> show handle <> ". Probably xdg-desktop-portal is too old."
               in throwIO (DBus.clientError msg)
    _ ->
      throwIO (DBus.clientError ("Request reply in unexpected format: " <> show reply))

-- | Call a method on the desktop portal D-Bus object, and read the response directly
-- rather than asynchronously via a request object.
callMethod ::
  Client ->
  -- | Which portal interface to invoke.
  InterfaceName ->
  -- | Which method to invoke on that interface.
  MemberName ->
  -- | Arguments to pass to the method.
  [Variant] ->
  -- | The response from the method call.
  IO [Variant]
callMethod client interface memberName methodCallBody = do
  let methodCall = (portalMethodCall interface memberName) {DBus.methodCallBody}
  DBus.methodReturnBody <$> DBus.call_ client.dbusClient methodCall

-- | Call a method on the desktop portal D-Bus object, but ignore the response.
callMethod_ ::
  Client ->
  -- | Which portal interface to invoke.
  InterfaceName ->
  -- | Which method to invoke on that interface.
  MemberName ->
  -- | Arguments to pass to the method.
  [Variant] ->
  IO ()
callMethod_ client interface memberName methodCallBody = do
  let methodCall = (portalMethodCall interface memberName) {DBus.methodCallBody}
  void (DBus.call client.dbusClient methodCall)

handleSignal :: Client -> InterfaceName -> MemberName -> ([Variant] -> IO ()) -> IO SignalHandler
handleSignal client interface memberName handler = do
  dbusSignalHandler <-
    DBus.addMatch
      client.dbusClient
      DBus.matchAny
        { matchInterface = Just interface,
          matchMember = Just memberName,
          matchDestination = Just client.clientName
        }
      (\Signal {signalBody} -> handler signalBody)
  pure SignalHandler {dbusSignalHandler, client}

-- | Prevent any future invocations of the given signal handler.
cancelSignalHandler :: SignalHandler -> IO ()
cancelSignalHandler handler =
  DBus.removeMatch handler.client.dbusClient handler.dbusSignalHandler

requestToken :: IO Text
requestToken = do
  (rnd :: Word64) <- R.uniformM R.globalStdGen
  pure ("haskell_desktop_portal_" <> pack (show rnd))

requestHandle :: BusName -> IO (ObjectPath, Text)
requestHandle clientName = do
  token <- requestToken
  pure (DBus.objectPath_ ("/org/freedesktop/portal/desktop/request/" <> escapeClientName clientName <> "/" <> unpack token), token)
  where
    escapeClientName =
      map (\case '.' -> '_'; c -> c) . drop 1 . DBus.formatBusName

portalMethodCall :: InterfaceName -> MemberName -> MethodCall
portalMethodCall interface memberName =
  (DBus.methodCall "/org/freedesktop/portal/desktop" interface memberName)
    { DBus.methodCallDestination = Just "org.freedesktop.portal.Desktop"
    }
