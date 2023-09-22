module Desktop.Portal.Camera
  ( accessCamera,
    openPipeWireRemote,
    isCameraPresent,
  )
where

import Control.Exception (throwIO)
import DBus (InterfaceName, Variant, fromVariant, toVariant)
import DBus.Client qualified as DBus
import Data.Map (Map)
import Data.Text (Text)
import Desktop.Portal.Internal (Client, Request, callMethod, getPropertyValue, sendRequest)
import System.Posix (Fd)

cameraInterface :: InterfaceName
cameraInterface = "org.freedesktop.portal.Camera"

-- | Requests access to the camera.
--
-- If access is granted, then request will return '()', otherwise it will be cancelled.
accessCamera :: Client -> IO (Request ())
accessCamera client = do
  sendRequest client cameraInterface "AccessCamera" [] mempty (const (pure ()))

openPipeWireRemote :: Client -> IO Fd
openPipeWireRemote client = do
  res <- callMethod client cameraInterface "OpenPipeWireRemote" [toVariant (mempty :: Map Text Variant)]
  case res of
    [fromVariant -> Just fd] ->
      pure fd
    _ ->
      throwIO . DBus.clientError $ "openPipeWireRemote: could not parse response: " <> show res

isCameraPresent :: Client -> IO Bool
isCameraPresent client = do
  getPropertyValue client cameraInterface "IsCameraPresent"
