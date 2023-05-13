module Desktop.Portal.SettingsSpec (spec) where

import Control.Monad (void)
import DBus (InterfaceName, toVariant)
import Data.Default.Class (Default (def))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Desktop.Portal.Settings (ColorScheme (..), ReadAllOptions (..), ReadAllResults (..), ReadOptions (..), ReadResults (..), SettingValue (..), StandardSetting (..))
import Desktop.Portal.Settings qualified as Settings
import Desktop.Portal.TestUtil
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn, shouldThrow)

settingsInterface :: InterfaceName
settingsInterface = "org.freedesktop.portal.Settings"

spec :: Spec
spec = do
  around withTestBus $ do
    describe "readAll" $ do
      it "should encode request" $ \handle -> do
        let responseBody = [toVariantMap []]
        body <- savingMethodArguments handle settingsInterface "ReadAll" responseBody $ do
          void (Settings.readAll (client handle) (ReadAllOptions ["ns1", "ns2"]))
        body
          `shouldBe` [toVariant ["ns1" :: Text, "ns2"]]

      it "should decode response" $ \handle -> do
        let responseBody =
              [ toVariant . Map.singleton ("namespace" :: Text) . Map.fromList $
                  [ ("key1" :: Text, toVariant (42 :: Word32)),
                    ("key2", toVariant True)
                  ]
              ]
        withMethodResponse handle settingsInterface "ReadAll" responseBody $ do
          Settings.readAll (client handle) def
            `shouldReturn` ReadAllResults
              [ SettingValue
                  { namespace = "namespace",
                    key = "key1",
                    value = toVariant (42 :: Word32),
                    standardValue = Nothing
                  },
                SettingValue
                  { namespace = "namespace",
                    key = "key2",
                    value = toVariant True,
                    standardValue = Nothing
                  }
              ]

      it "should fail to decode invalid response" $ \handle ->
        withMethodResponse handle settingsInterface "ReadAll" [] $ do
          Settings.readAll (client handle) def `shouldThrow` dbusClientException

    describe "read" $ do
      it "should encode request" $ \handle -> do
        let responseBody = [toVariantText "wibble"]
        body <- savingMethodArguments handle settingsInterface "Read" responseBody $ do
          void (Settings.read (client handle) (ReadOptions "namespace" "key"))
        body
          `shouldBe` [toVariantText "namespace", toVariantText "key"]

      it "should decode response" $ \handle -> do
        let responseBody = [toVariant (42 :: Word32)]
        withMethodResponse handle settingsInterface "Read" responseBody $ do
          Settings.read (client handle) (ReadOptions "ns" "key")
            `shouldReturn` ReadResults {value = toVariant (42 :: Word32), standardValue = Nothing}

      it "should fail to decode invalid response" $ \handle ->
        withMethodResponse handle settingsInterface "Read" [] $ do
          Settings.read (client handle) (ReadOptions "ns" "key") `shouldThrow` dbusClientException

    describe "standard settings" $ do
      it "should decode color scheme" $ \handle -> do
        let responseBody = [toVariant (2 :: Word32)]
        withMethodResponse handle settingsInterface "Read" responseBody $ do
          Settings.read (client handle) (ReadOptions "org.freedesktop.appearance" "color-scheme")
            `shouldReturn` ReadResults
              { value = toVariant (2 :: Word32),
                standardValue = Just (SettingColorScheme ColorSchemeLight)
              }
