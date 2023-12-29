{-# LANGUAGE QuasiQuotes #-}

module Desktop.Portal.AccountSpec (spec) where

import Control.Monad (void)
import DBus (InterfaceName)
import Data.Default.Class (Default (def))
import Desktop.Portal (GetUserInformationOptions (..), GetUserInformationResults (..))
import Desktop.Portal qualified as Portal
import Desktop.Portal.TestUtil
import System.OsPath (osp)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn, shouldThrow)

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
                  ("image", toVariantText "file:///some/path")
                ]
        withRequestResponse handle accountInterface "GetUserInformation" responseBody $ do
          (Portal.getUserInformation (client handle) def >>= Portal.await)
            `shouldReturn` Just (GetUserInformationResults "_id" "_name" (Just [osp|/some/path|]))

      it "should decode response without image" $ \handle -> do
        let responseBody =
              successResponse
                [ ("id", toVariantText "_id"),
                  ("name", toVariantText "_name")
                ]
        withRequestResponse handle accountInterface "GetUserInformation" responseBody $ do
          (Portal.getUserInformation (client handle) def >>= Portal.await)
            `shouldReturn` Just (GetUserInformationResults "_id" "_name" Nothing)

      it "should fail to decode invalid response" $ \handle ->
        withRequestResponse handle accountInterface "GetUserInformation" (successResponse []) $ do
          (Portal.getUserInformation (client handle) def >>= Portal.await) `shouldThrow` dbusClientException
