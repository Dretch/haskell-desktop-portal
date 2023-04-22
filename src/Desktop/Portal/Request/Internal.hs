module Desktop.Portal.Request.Internal
  ( Request,
    sendRequest,
    await,
    cancel,
  )
where

import Control.Concurrent (MVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Exception (SomeException, catch, onException, throwIO)
import Control.Monad (when)
import DBus (BusName, InterfaceName, MemberName, MethodCall, ObjectPath)
import DBus qualified
import DBus.Client (Client, MatchRule (..))
import DBus.Client qualified as DBus
import DBus.Internal.Message (Signal (..))
import DBus.Internal.Types (Variant)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack, unpack)
import Data.Word (Word32, Word64)
import System.Random.Stateful qualified as R

-- | A portal request that may be in-progress, finished, or cancelled.
data Request a = Request
  { client :: Client,
    methodCall :: MethodCall,
    result :: MVar (Either SomeException (Maybe a))
  }

instance Eq (Request a) where
  a == b = a.result == b.result

instance Show (Request a) where
  show request =
    "Request{client=<"
      <> show request.client.clientThreadID
      <> ">, methodCall="
      <> show request.methodCall
      <> ", result=<MVar>}"

-- | Wait for a request to be finished, and return the result if it succeeded. If the
-- request is cancelled, either by the user interface or by calling 'cancel', then
-- 'Nothing' will be returned.
await :: Request a -> IO (Maybe a)
await request = do
  readMVar request.result >>= \case
    Left exn -> throwIO exn
    Right res -> pure res

-- | Cancel a request. This will cause any threads blocked on 'await' to receive 'Nothing'.
cancel :: Request a -> IO ()
cancel request = do
  putSucceeded <- tryPutMVar request.result (Right Nothing)
  when putSucceeded $ do
    -- Otherwise the request was already finished/cancelled. Don't bother calling
    -- the Close method on the request because disconnection has the same effect.
    DBus.disconnect request.client

-- | Send a request to the desktop portal D-Bus object.
sendRequest ::
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
sendRequest interface memberName parameters options parseResponse = do
  (client, clientName) <- connect
  onException (sendRequest' client clientName) (DBus.disconnect client)
  where
    sendRequest' client clientName = do
      (handle, token) <- requestHandle clientName

      sentRequestVar <- newEmptyMVar
      resultVar <- newEmptyMVar

      -- listen before sending the request, to avoid a race condition where the
      -- response happens before we get a chance to register the listener for it
      _ <-
        DBus.addMatch
          client
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
              -- need to try because cancel might have been called and populated the mvar with Nothing
              putSucceeded <- tryPutMVar resultVar val
              -- we only use the connection for a single request, which is now done
              when putSucceeded $ do
                -- don't disconnect until we have got the response to the initial
                -- request method call, otherwise reading the response will fail
                takeMVar sentRequestVar
                DBus.disconnect client
          )

      let methodCall =
            (DBus.methodCall "/org/freedesktop/portal/desktop" interface memberName)
              { DBus.methodCallDestination = Just "org.freedesktop.portal.Desktop",
                DBus.methodCallBody =
                  parameters <> [DBus.toVariant (Map.insert "handle_token" (DBus.toVariant token) options)]
              }

      reply <- DBus.call_ client methodCall
      putMVar sentRequestVar ()
      case DBus.methodReturnBody reply of
        [x]
          | Just (objX :: ObjectPath) <- DBus.fromVariant x ->
              if objX == handle
                then pure (Request client methodCall resultVar)
                else
                  let msg = "Unexpected handle: " <> show objX <> " should be " <> show handle <> ". Probably xdg-desktop-portal is too old."
                   in throwIO (DBus.clientError msg)
        _ ->
          throwIO (DBus.clientError ("Request reply in unexpected format: " <> show reply))

connect :: IO (Client, BusName)
connect = do
  env <- DBus.getSessionAddress
  case env of
    Nothing -> throwIO (DBus.clientError "connect: session address not found.")
    Just addr -> DBus.connectWithName DBus.defaultClientOptions addr

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
