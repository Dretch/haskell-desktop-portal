{-# LANGUAGE QuasiQuotes #-}

module Desktop.Portal.OpenURISpec (spec) where

import Control.Monad (void)
import DBus (InterfaceName, IsVariant (toVariant))
import Desktop.Portal (FileSpec (..))
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
        withTempFd $ \fd -> do
          body <- savingRequestArguments handle openURIInterface "OpenFile" $ do
            void (OpenURI.openFile (client handle) (OpenURI.openFileOptions (FileSpecFd fd)))
          body
            `shouldSatisfyList` [ (== toVariantText ""),
                                  isDifferentUnixFd fd,
                                  (== toVariantMap [])
                                ]
      it "should encode request with all Justs" $ \handle -> do
        withTempFd $ \fd -> do
          body <- savingRequestArguments handle openURIInterface "OpenFile" $ do
            void
              ( OpenURI.openFile
                  (client handle)
                  ( OpenFileOptions
                      { fileSpec = FileSpecFd fd,
                        parentWindow = Just "_window",
                        writable = Just True,
                        ask = Just True,
                        activationToken = Just "_token"
                      }
                  )
              )
          body
            `shouldSatisfyList` [ (== toVariantText "_window"),
                                  isDifferentUnixFd fd,
                                  ( ==
                                      toVariantMap
                                        [ ("writable", toVariant True),
                                          ("ask", toVariant True),
                                          ("activation_token", toVariantText "_token")
                                        ]
                                  )
                                ]

      it "should encode request with file path" $ \handle -> do
        withTempFilePath $ \path -> do
          body <- savingRequestArguments handle openURIInterface "OpenFile" $ do
            void (OpenURI.openFile (client handle) (OpenURI.openFileOptions (FileSpecPath path)))
          body
            `shouldSatisfyList` [ (== toVariantText ""),
                                  isUnixFd,
                                  (== toVariantMap [])
                                ]

      it "should decode response" $ \handle -> do
        withTempFd $ \fd -> do
          withRequestResponse handle openURIInterface "OpenFile" (successResponse []) $ do
            (OpenURI.openFile (client handle) (OpenURI.openFileOptions (FileSpecFd fd)) >>= Portal.await)
              `shouldReturn` Just ()

    describe "openDirectory" $ do
      it "should encode request with all Nothings" $ \handle -> do
        withTempDirectoryFd $ \fd -> do
          body <- savingRequestArguments handle openURIInterface "OpenDirectory" $ do
            void (Portal.openDirectory (client handle) (Portal.openDirectoryOptions (FileSpecFd fd)))
          body
            `shouldSatisfyList` [ (== toVariantText ""),
                                  isDifferentUnixFd fd,
                                  (== toVariantMap [])
                                ]
      it "should encode request with all Justs" $ \handle -> do
        withTempDirectoryFd $ \fd -> do
          body <- savingRequestArguments handle openURIInterface "OpenDirectory" $ do
            void
              ( Portal.openDirectory
                  (client handle)
                  ( OpenDirectoryOptions
                      { fileSpec = FileSpecFd fd,
                        parentWindow = Just "_window",
                        activationToken = Just "_token"
                      }
                  )
              )
          body
            `shouldSatisfyList` [ (== toVariantText "_window"),
                                  isDifferentUnixFd fd,
                                  (== toVariantMap [("activation_token", toVariantText "_token")])
                                ]

      it "should encode request with file path" $ \handle -> do
        withTempFilePath $ \path -> do
          body <- savingRequestArguments handle openURIInterface "OpenDirectory" $ do
            void (OpenURI.openDirectory (client handle) (OpenURI.openDirectoryOptions (FileSpecPath path)))
          body
            `shouldSatisfyList` [ (== toVariantText ""),
                                  isUnixFd,
                                  (== toVariantMap [])
                                ]

      it "should decode response" $ \handle -> do
        withTempFd $ \fd -> do
          withRequestResponse handle openURIInterface "OpenDirectory" (successResponse []) $ do
            (Portal.openDirectory (client handle) (Portal.openDirectoryOptions (FileSpecFd fd)) >>= Portal.await)
              `shouldReturn` Just ()
