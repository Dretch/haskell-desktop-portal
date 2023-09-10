module Desktop.Portal.Secret (retrieveSecret) where

import Control.Exception (finally, throwIO)
import DBus (InterfaceName, IsVariant (toVariant))
import DBus.Client qualified as DBus
import Data.ByteString (ByteString)
import Desktop.Portal.Internal (Client, await, sendRequest)
import Network.Socket (Family (..), SocketType (..), close, defaultProtocol, socketPair, withFdSocket)
import Network.Socket.ByteString (recv)
import System.Posix (Fd (..))
 
secretsInterface :: InterfaceName
secretsInterface = "org.freedesktop.portal.Secret"

-- | Retrieve the application-specific secret.
-- 
-- Currently works in Gnome, but not KDE (see https://bugs.kde.org/show_bug.cgi?id=466197 ).
-- The token parameter that is documented in the portal API specs is not supported either, as
-- it is not clear exactly how this should work and it does not seem to be supported by Gnome.
retrieveSecret :: Client -> IO ByteString
retrieveSecret client = do
  (r, w) <- socketPair AF_UNIX Stream defaultProtocol
  flip finally (close r) $ do
    flip finally (close w) $ do
      withFdSocket w $ \fdInt -> do
        let fd = toVariant (Fd fdInt)
        req <- sendRequest client secretsInterface "RetrieveSecret" [fd] mempty pure
        await req >>= \case
          Nothing ->
            throwIO . DBus.clientError $ "retrieveSecret: request was cancelled."
          Just _tokens -> do
            recv r 4096
