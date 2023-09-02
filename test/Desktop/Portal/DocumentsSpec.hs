module Desktop.Portal.DocumentsSpec (spec) where

import Control.Monad (void)
import DBus (BusName, InterfaceName, MemberName, ObjectPath, Variant, toVariant)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
import Desktop.Portal (FileSpec (..))
import Desktop.Portal.Documents (AddFlag (..), ExtraResults (..), GrantPermission (..))
import Desktop.Portal.Documents qualified as Documents
import Desktop.Portal.TestUtil
import Desktop.Portal.TestUtil qualified as DBus
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn, shouldSatisfy)

documentsInterface :: InterfaceName
documentsInterface = "org.freedesktop.portal.Documents"

documentsBusName :: BusName
documentsBusName = "org.freedesktop.portal.Documents"

documentsObject :: ObjectPath
documentsObject = "/org/freedesktop/portal/documents"

spec :: Spec
spec = do
  around (withTestBus_ documentsBusName) $ do
    describe "getMountPoint" $ do
      it "should encode request" $ \handle -> do
        let responseBody = [toVariant ("/\0" :: ByteString)]
        body <- savingDocumentsMethodArguments handle "GetMountPoint" responseBody $ do
          void $ Documents.getMountPoint (client handle)
        body `shouldBe` []

      it "should decode response" $ \handle -> do
        let responseBody = [toVariant ("/a/b/c\0" :: ByteString)]
        withDocumentsMethodResponse handle "GetMountPoint" responseBody $ do
          Documents.getMountPoint (client handle) `shouldReturn` "/a/b/c"

    describe "add" $ do
      it "should encode request with file descriptor" $ \handle -> do
        let responseBody = [toVariantText "docId"]
        withTempFd $ \fd -> do
          body <- savingDocumentsMethodArguments handle "Add" responseBody $ do
            void $ Documents.add (client handle) (FileSpecFd fd) False True
          head body `shouldSatisfy` isDifferentUnixFd fd
          tail body `shouldBe` [DBus.toVariant False, DBus.toVariant True]

      it "should encode request with file path" $ \handle -> do
        let responseBody = [toVariantText "docId"]
        withTempFilePath $ \path -> do
          body <- savingDocumentsMethodArguments handle "Add" responseBody $ do
            void $ Documents.add (client handle) (FileSpecPath path) False True
          head body `shouldSatisfy` isUnixFd
          tail body `shouldBe` [DBus.toVariant False, DBus.toVariant True]

      it "should decode response" $ \handle -> do
        let responseBody = [toVariantText "docId"]
        withTempFd $ \fd -> do
          withDocumentsMethodResponse handle "Add" responseBody $ do
            Documents.add (client handle) (FileSpecFd fd) False False
              `shouldReturn` "docId"

    describe "addFull" $ do
      it "should encode request with file descriptors" $ \handle -> do
        withTempFds 7 $ \fds -> do
          let responseBody =
                [ DBus.toVariant ["docId" :: Text],
                  toVariantMap [("mountpoint", DBus.toVariant ("/\0" :: ByteString))]
                ]
          body <- savingDocumentsMethodArguments handle "AddFull" responseBody $ do
            void $
              Documents.addFull
                (client handle)
                (FileSpecFd <$> fds)
                [AddReuseExisting, AddPersistent, AddAsNeededByApp, AddExportDirectory]
                (Just "appId")
                [GrantRead, GrantWrite, GrantGrantPermissions, GrantDelete]
          head body `shouldSatisfy` isDifferentUnixFds fds
          tail body
            `shouldBe` [ DBus.toVariant (0b1111 :: Word32),
                         toVariantText "appId",
                         DBus.toVariant ["read" :: Text, "write", "grant-permissions", "delete"]
                       ]

      it "should encode request with file paths" $ \handle -> do
        withTempFilePaths 8 $ \paths -> do
          let responseBody =
                [ DBus.toVariant ["docId" :: Text],
                  toVariantMap [("mountpoint", DBus.toVariant ("/\0" :: ByteString))]
                ]
          body <- savingDocumentsMethodArguments handle "AddFull" responseBody $ do
            void $
              Documents.addFull (client handle) (FileSpecPath <$> paths) [] Nothing []
          head body `shouldSatisfy` isUnixFds 8
          tail body
            `shouldBe` [DBus.toVariant (0 :: Word32), toVariantText "", DBus.toVariant ([] :: [Text])]

      it "should decode response" $ \handle -> do
        let responseBody =
              [ DBus.toVariant ["docId" :: Text],
                toVariantMap [("mountpoint", DBus.toVariant ("/a/b/c\0" :: ByteString))]
              ]
        withDocumentsMethodResponse handle "AddFull" responseBody $ do
          Documents.addFull (client handle) [] [] Nothing []
            `shouldReturn` (["docId"], ExtraResults "/a/b/c")

    describe "addNamed" $ do
      it "should encode request with file descriptor" $ \handle -> do
        withTempDirectoryFd $ \fd -> do
          let responseBody = [toVariantText "docId"]
          body <- savingDocumentsMethodArguments handle "AddNamed" responseBody $ do
            void $ Documents.addNamed (client handle) (FileSpecFd fd) "filename" False True
          head body `shouldSatisfy` isDifferentUnixFd fd
          tail body
            `shouldBe` [ DBus.toVariant ("filename\0" :: ByteString),
                         DBus.toVariant False,
                         DBus.toVariant True
                       ]

      it "should encode request with file path" $ \handle -> do
        withTempDirectoryFilePath $ \path -> do
          let responseBody = [toVariantText "docId"]
          body <- savingDocumentsMethodArguments handle "AddNamed" responseBody $ do
            void $ Documents.addNamed (client handle) (FileSpecPath path) "filename" False True
          head body `shouldSatisfy` isUnixFd
          tail body
            `shouldBe` [ DBus.toVariant ("filename\0" :: ByteString),
                         DBus.toVariant False,
                         DBus.toVariant True
                       ]

      it "should decode response" $ \handle -> do
        withTempDirectoryFd $ \fd -> do
          let responseBody = [DBus.toVariantText "docId"]
          withDocumentsMethodResponse handle "AddNamed" responseBody $ do
            Documents.addNamed (client handle) (FileSpecFd fd) "filename\0" False False
              `shouldReturn` "docId"

    describe "addNamedFull" $ do
      it "should encode request with file descriptor" $ \handle -> do
        withTempDirectoryFd $ \fd -> do
          let responseBody =
                [ DBus.toVariantText "docId",
                  toVariantMap [("mountpoint", DBus.toVariant ("/\0" :: ByteString))]
                ]
          body <- savingDocumentsMethodArguments handle "AddNamedFull" responseBody $ do
            void $
              Documents.addNamedFull
                (client handle)
                (FileSpecFd fd)
                "filename"
                [AddReuseExisting, AddPersistent, AddAsNeededByApp, AddExportDirectory]
                (Just "appId")
                [GrantRead, GrantWrite, GrantGrantPermissions, GrantDelete]
          head body `shouldSatisfy` isDifferentUnixFd fd
          tail body
            `shouldBe` [ DBus.toVariant ("filename\0" :: ByteString),
                         DBus.toVariant (0b1111 :: Word32),
                         toVariantText "appId",
                         DBus.toVariant ["read" :: Text, "write", "grant-permissions", "delete"]
                       ]

      it "should encode request with file path" $ \handle -> do
        withTempDirectoryFilePath $ \path -> do
          let responseBody =
                [ DBus.toVariantText "docId",
                  toVariantMap [("mountpoint", DBus.toVariant ("/\0" :: ByteString))]
                ]
          body <- savingDocumentsMethodArguments handle "AddNamedFull" responseBody $ do
            void $
              Documents.addNamedFull (client handle) (FileSpecPath path) "filename" [] Nothing []
          head body `shouldSatisfy` isUnixFd
          tail body
            `shouldBe` [ DBus.toVariant ("filename\0" :: ByteString),
                         DBus.toVariant (0 :: Word32),
                         toVariantText "",
                         DBus.toVariant ([] :: [Text])
                       ]

      it "should decode response" $ \handle -> do
        withTempDirectoryFd $ \fd -> do
          let responseBody =
                [ DBus.toVariantText "docId",
                  toVariantMap [("mountpoint", DBus.toVariant ("/a/b/c\0" :: ByteString))]
                ]
          withDocumentsMethodResponse handle "AddNamedFull" responseBody $ do
            Documents.addNamedFull (client handle) (FileSpecFd fd) "filename\0" [] Nothing []
              `shouldReturn` ("docId", ExtraResults "/a/b/c")

    describe "grantPermissions" $ do
      it "should encode request" $ \handle -> do
        body <- savingDocumentsMethodArguments handle "GrantPermissions" [] $ do
          Documents.grantPermissions
            (client handle)
            "docId"
            "appId"
            [GrantRead, GrantWrite, GrantGrantPermissions, GrantDelete]
        body
          `shouldBe` [ toVariantText "docId",
                       toVariantText "appId",
                       DBus.toVariant ["read" :: Text, "write", "grant-permissions", "delete"]
                     ]

    describe "revokePermissions" $ do
      it "should encode request" $ \handle -> do
        body <- savingDocumentsMethodArguments handle "RevokePermissions" [] $ do
          Documents.revokePermissions
            (client handle)
            "docId"
            "appId"
            [GrantRead, GrantWrite, GrantGrantPermissions, GrantDelete]
        body
          `shouldBe` [ toVariantText "docId",
                       toVariantText "appId",
                       DBus.toVariant ["read" :: Text, "write", "grant-permissions", "delete"]
                     ]

    describe "delete" $ do
      it "should encode request" $ \handle -> do
        body <- savingDocumentsMethodArguments handle "Delete" [] $ do
          Documents.delete (client handle) "docId"
        body `shouldBe` [toVariantText "docId"]

savingDocumentsMethodArguments :: TestHandle -> MemberName -> [Variant] -> IO () -> IO [Variant]
savingDocumentsMethodArguments handle =
  savingMethodArguments_ handle documentsObject documentsInterface

withDocumentsMethodResponse :: TestHandle -> MemberName -> [Variant] -> IO () -> IO ()
withDocumentsMethodResponse handle =
  withMethodResponse_ handle documentsObject documentsInterface
