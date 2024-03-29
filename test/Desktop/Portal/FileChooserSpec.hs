{-# LANGUAGE QuasiQuotes #-}

module Desktop.Portal.FileChooserSpec (spec) where

import Control.Monad (void)
import DBus (InterfaceName, toVariant)
import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Data.Text (Text)
import Data.Word (Word32)
import Desktop.Portal (ChoiceCombo (..), ChoiceComboOption (..), ChoiceComboSelection (..), Filter (..), FilterFileType (..), OpenFileOptions (..), OpenFileResults (..), SaveFileOptions (..), SaveFileResults (..))
import Desktop.Portal qualified as Portal
import Desktop.Portal.TestUtil
import System.OsPath (osp)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn, shouldThrow)

fileChooserInterface :: InterfaceName
fileChooserInterface = "org.freedesktop.portal.FileChooser"

spec :: Spec
spec = do
  around withTestBus $ do
    describe "openFile" $ do
      it "should encode request with all Nothings" $ \handle -> do
        body <- savingRequestArguments handle fileChooserInterface "OpenFile" $ do
          void (Portal.openFile (client handle) def)
        body
          `shouldBe` [ toVariantText "",
                       toVariantText "",
                       toVariantMap []
                     ]

      it "should encode request with all Justs" $ \handle -> do
        body <- savingRequestArguments handle fileChooserInterface "OpenFile" $ do
          void . Portal.openFile (client handle) $
            OpenFileOptions
              { parentWindow = Just "_parentWindow",
                title = Just "_title",
                acceptLabel = Just "_acceptLabel",
                modal = Just True,
                multiple = Just True,
                directory = Just True,
                filters =
                  Just
                    [ Filter
                        { name = "_filterName",
                          fileTypes = [GlobFilter "*.txt", MimeFilter "application/json"]
                        }
                    ],
                currentFilter =
                  Just
                    ( Filter
                        { name = "_currentFilterName",
                          fileTypes = [GlobFilter "*.jpg", MimeFilter "image/jpeg"]
                        }
                    ),
                choices =
                  Just
                    [ ChoiceCombo
                        { id = "_choiceId",
                          label_ = "_choiceLabel",
                          choices = [ChoiceComboOption {id = "_ccoId", label_ = "_ccoLabel"}],
                          defaultChoiceId = "_defaultChoiceId"
                        }
                    ]
              }
        body
          `shouldBe` [ toVariantText "_parentWindow",
                       toVariantText "_title",
                       toVariantMap
                         [ ("accept_label", toVariantText "_acceptLabel"),
                           ("modal", toVariant True),
                           ("multiple", toVariant True),
                           ("directory", toVariant True),
                           ( "filters",
                             toVariant
                               [ ( "_filterName" :: Text,
                                   [ (0 :: Word32, "*.txt" :: Text),
                                     (1 :: Word32, "application/json" :: Text)
                                   ]
                                 )
                               ]
                           ),
                           ( "current_filter",
                             toVariant
                               ( "_currentFilterName" :: Text,
                                 [ (0 :: Word32, "*.jpg" :: Text),
                                   (1 :: Word32, "image/jpeg" :: Text)
                                 ]
                               )
                           ),
                           ( "choices",
                             toVariant
                               [ ( "_choiceId" :: Text,
                                   "_choiceLabel" :: Text,
                                   [("_ccoId" :: Text, "_ccoLabel" :: Text)],
                                   "_defaultChoiceId" :: Text
                                 )
                               ]
                           )
                         ]
                     ]

      it "should decode response with all Nothings" $ \handle -> do
        let responseBody = successResponse [("uris", toVariant ["file:///a/b/c" :: Text])]
        withRequestResponse handle fileChooserInterface "OpenFile" responseBody $ do
          (Portal.openFile (client handle) def >>= Portal.await)
            `shouldReturn` Just
              (OpenFileResults {uris = [[osp|/a/b/c|]], choices = Nothing, currentFilter = Nothing})

      it "should decode response with all Justs" $ \handle -> do
        let responseBody =
              successResponse
                [ ("uris", toVariant ["file:///a/b/c" :: Text]),
                  ("choices", toVariant [("_comboId" :: Text, "_optionId" :: Text)]),
                  ("current_filter", toVariant ("_filterId" :: Text, [(0 :: Word32, "*.md" :: Text)]))
                ]
        withRequestResponse handle fileChooserInterface "OpenFile" responseBody $ do
          (Portal.openFile (client handle) def >>= Portal.await)
            `shouldReturn` Just
              ( OpenFileResults
                  { uris = [[osp|/a/b/c|]],
                    choices = Just [ChoiceComboSelection {comboId = "_comboId", optionId = "_optionId"}],
                    currentFilter = Just Filter {name = "_filterId", fileTypes = [GlobFilter "*.md"]}
                  }
              )

      it "should fail to decode invalid response" $ \handle ->
        withRequestResponse handle fileChooserInterface "OpenFile" (successResponse []) $ do
          (Portal.openFile (client handle) def >>= Portal.await) `shouldThrow` dbusClientException

    describe "saveFile" $ do
      it "should encode request with all Nothings" $ \handle -> do
        body <- savingRequestArguments handle fileChooserInterface "SaveFile" $ do
          void (Portal.saveFile (client handle) def)
        body
          `shouldBe` [ toVariantText "",
                       toVariantText "",
                       toVariantMap []
                     ]

      it "should encode request with all Justs" $ \handle -> do
        body <- savingRequestArguments handle fileChooserInterface "SaveFile" $ do
          void . Portal.saveFile (client handle) $
            SaveFileOptions
              { parentWindow = Just "_parentWindow",
                title = Just "_title",
                acceptLabel = Just "_acceptLabel",
                modal = Just True,
                filters =
                  Just
                    [ Filter
                        { name = "_filterName",
                          fileTypes = [GlobFilter "*.txt", MimeFilter "application/json"]
                        }
                    ],
                currentFilter =
                  Just
                    ( Filter
                        { name = "_currentFilterName",
                          fileTypes = [GlobFilter "*.jpg", MimeFilter "image/jpeg"]
                        }
                    ),
                choices =
                  Just
                    [ ChoiceCombo
                        { id = "_choiceId",
                          label_ = "_choiceLabel",
                          choices = [ChoiceComboOption {id = "_ccoId", label_ = "_ccoLabel"}],
                          defaultChoiceId = "_defaultChoiceId"
                        }
                    ],
                currentName = Just "some_name",
                currentFolder = Just [osp|/some/folder|],
                currentFile = Just [osp|/some/file|]
              }
        body
          `shouldBe` [ toVariantText "_parentWindow",
                       toVariantText "_title",
                       toVariantMap
                         [ ("accept_label", toVariantText "_acceptLabel"),
                           ("modal", toVariant True),
                           ( "filters",
                             toVariant
                               [ ( "_filterName" :: Text,
                                   [ (0 :: Word32, "*.txt" :: Text),
                                     (1 :: Word32, "application/json" :: Text)
                                   ]
                                 )
                               ]
                           ),
                           ( "current_filter",
                             toVariant
                               ( "_currentFilterName" :: Text,
                                 [ (0 :: Word32, "*.jpg" :: Text),
                                   (1 :: Word32, "image/jpeg" :: Text)
                                 ]
                               )
                           ),
                           ( "choices",
                             toVariant
                               [ ( "_choiceId" :: Text,
                                   "_choiceLabel" :: Text,
                                   [("_ccoId" :: Text, "_ccoLabel" :: Text)],
                                   "_defaultChoiceId" :: Text
                                 )
                               ]
                           ),
                           ("current_name", toVariantText "some_name"),
                           ("current_folder", toVariant ("/some/folder\0" :: ByteString)),
                           ("current_file", toVariant ("/some/file\0" :: ByteString))
                         ]
                     ]

      it "should decode response with all Nothings" $ \handle -> do
        let responseBody = successResponse [("uris", toVariant ["file:///a/b/c" :: Text])]
        withRequestResponse handle fileChooserInterface "SaveFile" responseBody $ do
          (Portal.saveFile (client handle) def >>= Portal.await)
            `shouldReturn` Just
              (SaveFileResults {uris = [[osp|/a/b/c|]], choices = Nothing, currentFilter = Nothing})

      it "should decode response with all Justs" $ \handle -> do
        let responseBody =
              successResponse
                [ ("uris", toVariant ["file:///a/b/c" :: Text]),
                  ("choices", toVariant [("_comboId" :: Text, "_optionId" :: Text)]),
                  ("current_filter", toVariant ("_filterId" :: Text, [(0 :: Word32, "*.md" :: Text)]))
                ]
        withRequestResponse handle fileChooserInterface "SaveFile" responseBody $ do
          (Portal.saveFile (client handle) def >>= Portal.await)
            `shouldReturn` Just
              ( SaveFileResults
                  { uris = [[osp|/a/b/c|]],
                    choices = Just [ChoiceComboSelection {comboId = "_comboId", optionId = "_optionId"}],
                    currentFilter = Just Filter {name = "_filterId", fileTypes = [GlobFilter "*.md"]}
                  }
              )

      it "should fail to decode invalid response" $ \handle ->
        withRequestResponse handle fileChooserInterface "SaveFile" (successResponse []) $ do
          (Portal.openFile (client handle) def >>= Portal.await) `shouldThrow` dbusClientException
