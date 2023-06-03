{-# LANGUAGE QuasiQuotes #-}

module Desktop.Portal.OpenURISpec (spec) where

import Control.Monad (void)
import DBus (InterfaceName, IsVariant (toVariant))
import Data.Word (Word32)
import Desktop.Portal qualified as Portal
import Desktop.Portal.OpenURI (OpenDirectoryOptions (..), OpenFileOptions (..), OpenURIOptions (..))
import Desktop.Portal.OpenURI qualified as OpenURI
import Desktop.Portal.TestUtil
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn)
import Text.URI.QQ (uri)

openURIInterface :: InterfaceName
openURIInterface = "org.freedesktop.portal.OpenURI"

spec :: Spec
spec = do
  around withTestBus $ do
    describe "openURI" $ do
      it "should encode request with all Nothings" $ \handle -> do
        body <- savingRequestArguments handle openURIInterface "OpenURI" $ do
          void (Portal.openURI (client handle) (Portal.openURIOptions [uri|https://example.com|]))
        body
          `shouldBe` [ toVariantText "",
                       toVariantText "https://example.com",
                       toVariantMap []
                     ]

      it "should encode request with all Justs" $ \handle -> do
        body <- savingRequestArguments handle openURIInterface "OpenURI" $ do
          void . Portal.openURI (client handle) $
            OpenURIOptions
              { uri = [uri|https://example.com|],
                parentWindow = Just "_window",
                writable = Just True,
                ask = Just True,
                activationToken = Just "_token"
              }
        body
          `shouldBe` [ toVariantText "_window",
                       toVariantText "https://example.com",
                       toVariantMap
                         [ ("writable", toVariant True),
                           ("ask", toVariant True),
                           ("activation_token", toVariantText "_token")
                         ]
                     ]

      it "should decode response" $ \handle -> do
        withRequestResponse handle openURIInterface "OpenURI" (successResponse []) $ do
          (Portal.openURI (client handle) (Portal.openURIOptions [uri|https://example.com|]) >>= Portal.await)
            `shouldReturn` Just ()

    describe "openFile" $ do
      it "should encode request with all Nothings" $ \handle -> do
        body <- savingRequestArguments handle openURIInterface "OpenFile" $ do
          void (OpenURI.openFile (client handle) (OpenURI.openFileOptions 42))
        body
          `shouldBe` [ toVariantText "",
                       toVariant (42 :: Word32),
                       toVariantMap []
                     ]
      it "should encode request with all Justs" $ \handle -> do
        body <- savingRequestArguments handle openURIInterface "OpenFile" $ do
          void
            ( OpenURI.openFile
                (client handle)
                ( OpenFileOptions
                    { fd = 43,
                      parentWindow = Just "_window",
                      writable = Just True,
                      ask = Just True,
                      activationToken = Just "_token"
                    }
                )
            )
        body
          `shouldBe` [ toVariantText "_window",
                       toVariant (43 :: Word32),
                       toVariantMap
                         [ ("writable", toVariant True),
                           ("ask", toVariant True),
                           ("activation_token", toVariantText "_token")
                         ]
                     ]

      it "should decode response" $ \handle -> do
        withRequestResponse handle openURIInterface "OpenFile" (successResponse []) $ do
          (OpenURI.openFile (client handle) (OpenURI.openFileOptions 44) >>= Portal.await)
            `shouldReturn` Just ()

    describe "openDirectory" $ do
      it "should encode request with all Nothings" $ \handle -> do
        body <- savingRequestArguments handle openURIInterface "OpenDirectory" $ do
          void (Portal.openDirectory (client handle) (Portal.openDirectoryOptions 42))
        body
          `shouldBe` [ toVariantText "",
                       toVariant (42 :: Word32),
                       toVariantMap []
                     ]
      it "should encode request with all Justs" $ \handle -> do
        body <- savingRequestArguments handle openURIInterface "OpenDirectory" $ do
          void
            ( Portal.openDirectory
                (client handle)
                ( OpenDirectoryOptions
                    { fd = 43,
                      parentWindow = Just "_window",
                      activationToken = Just "_token"
                    }
                )
            )
        body
          `shouldBe` [ toVariantText "_window",
                       toVariant (43 :: Word32),
                       toVariantMap [("activation_token", toVariantText "_token")]
                     ]

      it "should decode response" $ \handle -> do
        withRequestResponse handle openURIInterface "OpenDirectory" (successResponse []) $ do
          (Portal.openDirectory (client handle) (Portal.openDirectoryOptions 44) >>= Portal.await)
            `shouldReturn` Just ()
