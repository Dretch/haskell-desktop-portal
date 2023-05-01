module Desktop.Portal.AccountSpec (spec) where

import Control.Monad (void)
import DBus (InterfaceName)
import Data.Default.Class (Default (def))
import Desktop.Portal (GetUserInformationOptions (..), GetUserInformationResults (..))
import Desktop.Portal qualified as Portal
import Desktop.Portal.TestUtil
import Test.Hspec (Spec, anyException, around, describe, it, shouldBe, shouldThrow)

accountInterface :: InterfaceName
accountInterface = "org.freedesktop.portal.Account"

spec :: Spec
spec = do
  around withTestBus $ do
    describe "getUserInformation" $ do
      it "should encode request with all Nothings" $ \handle -> do
        body <- savingRequestArguments handle accountInterface "GetUserInformation" $ do
          void (Portal.getUserInformation (client handle) (GetUserInformationOptions Nothing Nothing))
        body
          `shouldBe` [ toVariantText "",
                       toVariantMap []
                     ]

      it "should encode request with all Justs" $ \handle -> do
        body <- savingRequestArguments handle accountInterface "GetUserInformation" $ do
          void (Portal.getUserInformation (client handle) (GetUserInformationOptions (Just "_window") (Just "_reason")))
        body
          `shouldBe` [ toVariantText "_window",
                       toVariantMap [("reason", toVariantText "_reason")]
                     ]

      it "should decode response with image" $ \handle -> do
        let responseBody =
              successResponse
                [ ("id", toVariantText "_id"),
                  ("name", toVariantText "_name"),
                  ("image", toVariantText "_img")
                ]
        withMethodResponse handle accountInterface "GetUserInformation" responseBody $ do
          info <- Portal.getUserInformation (client handle) def >>= Portal.await
          info `shouldBe` Just (GetUserInformationResults "_id" "_name" (Just "_img"))

      it "should decode response without image" $ \handle -> do
        let responseBody =
              successResponse
                [ ("id", toVariantText "_id"),
                  ("name", toVariantText "_name")
                ]
        withMethodResponse handle accountInterface "GetUserInformation" responseBody $ do
          info <- Portal.getUserInformation (client handle) def >>= Portal.await
          info `shouldBe` Just (GetUserInformationResults "_id" "_name" Nothing)

      it "should fail to decode invalid response" $ \handle ->
        withMethodResponse handle accountInterface "GetUserInformation" (successResponse []) $ do
          (Portal.getUserInformation (client handle) def >>= Portal.await) `shouldThrow` anyException
