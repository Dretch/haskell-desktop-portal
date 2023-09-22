module Desktop.Portal.CameraSpec (spec) where

import DBus (InterfaceName, IsVariant (toVariant))
import Desktop.Portal qualified as Portal
import Desktop.Portal.TestUtil
import Test.Hspec (Spec, around, describe, it, shouldReturn, shouldThrow)
import Test.Hspec.Expectations (shouldSatisfy)

cameraInterface :: InterfaceName
cameraInterface = "org.freedesktop.portal.Camera"

spec :: Spec
spec = do
  around withTestBus $ do
    describe "accessCamera" $ do
      it "should decode response" $ \handle -> do
        let responseBody = successResponse []
        withRequestResponse handle cameraInterface "AccessCamera" responseBody $ do
          (Portal.accessCamera (client handle) >>= Portal.await)
            `shouldReturn` Just ()

    describe "openPipeWireRemote" $ do
      it "should decode response" $ \handle -> do
        withTempFd $ \fd -> do
          withMethodResponse handle cameraInterface "OpenPipeWireRemote" [toVariant fd] $ do
            fd' <- Portal.openPipeWireRemote (client handle)
            fd' `shouldSatisfy` (/=) fd

      it "should fail to decode invalid response" $ \handle -> do
        withMethodResponse handle cameraInterface "OpenPipeWireRemote" [toVariant False] $ do
          Portal.openPipeWireRemote (client handle)
            `shouldThrow` dbusClientException

    describe "isCameraPresent" $ do
      it "should get property" $ \handle -> do
        withReadOnlyProperty handle cameraInterface "IsCameraPresent" (pure True) $ do
          Portal.isCameraPresent (client handle)
            `shouldReturn` True
