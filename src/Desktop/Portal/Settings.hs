module Desktop.Portal.Settings
  ( -- * Common Types
    SettingValue (..),
    StandardSetting (..),
    ColorScheme (..),

    -- * Read All
    ReadAllOptions (..),
    ReadAllResults (..),
    readAll,

    -- * Read
    ReadOptions (..),
    ReadResults (..),
    read,
  )
where

import Control.Exception (throwIO)
import DBus (InterfaceName, Variant)
import DBus qualified
import DBus.Client qualified as DBus
import Data.Default.Class (Default (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Desktop.Portal.Internal (Client, callMethod)
import Prelude hiding (read)

newtype ReadAllOptions = ReadAllOptions
  {namespaces :: [Text]}
  deriving (Eq, Show)

instance Default ReadAllOptions where
  def = ReadAllOptions {namespaces = []}

newtype ReadAllResults = ReadAllResults
  {values :: [SettingValue]}
  deriving (Eq, Show)

data ReadOptions = ReadOptions
  { namespace :: Text,
    key :: Text
  }
  deriving (Eq, Show)

data ReadResults = ReadResults
  { value :: Variant,
    standardValue :: Maybe StandardSetting
  }
  deriving (Eq, Show)

data SettingValue = SettingValue
  { namespace :: Text,
    key :: Text,
    value :: Variant,
    standardValue :: Maybe StandardSetting
  }
  deriving (Eq, Show)

newtype StandardSetting
  = SettingColorScheme ColorScheme
  deriving (Eq, Show)

data ColorScheme
  = ColorSchemeNoPreference
  | ColorSchemeDark
  | ColorSchemeLight
  deriving (Eq, Show)

settingsInterface :: InterfaceName
settingsInterface = "org.freedesktop.portal.Settings"

readAll :: Client -> ReadAllOptions -> IO ReadAllResults
readAll client options =
  callMethod client settingsInterface "ReadAll" args >>= parseResponse
  where
    args = [DBus.toVariant options.namespaces]
    parseResponse = \case
      [resVal] | Just namespaceKeyMap <- DBus.fromVariant resVal -> do
        pure . ReadAllResults $
          flip Map.foldMapWithKey namespaceKeyMap $ \namespace keyMap ->
            flip Map.foldMapWithKey keyMap $ \key value ->
              [SettingValue {namespace, key, value, standardValue = decodeStandardSetting namespace key value}]
      res ->
        throwIO . DBus.clientError $ "readAll: could not parse response: " <> show res

read :: Client -> ReadOptions -> IO ReadResults
read client options =
  callMethod client settingsInterface "Read" args >>= parseResponse
  where
    args = [DBus.toVariant options.namespace, DBus.toVariant options.key]
    parseResponse = \case
      [value] ->
        pure ReadResults {value, standardValue = decodeStandardSetting options.namespace options.key value}
      res ->
        throwIO . DBus.clientError $ "read: could not parse response: " <> show res

decodeStandardSetting :: Text -> Text -> Variant -> Maybe StandardSetting
decodeStandardSetting namespace key value =
  case (namespace, key) of
    ("org.freedesktop.appearance", "color-scheme") -> Just (SettingColorScheme (decodeColorScheme value))
    _ -> Nothing
  where
    decodeColorScheme scheme
      | scheme == DBus.toVariant (1 :: Word32) = ColorSchemeDark
      | scheme == DBus.toVariant (2 :: Word32) = ColorSchemeLight
      | otherwise = ColorSchemeNoPreference
