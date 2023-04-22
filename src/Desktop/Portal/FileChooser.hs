module Desktop.Portal.FileChooser
  ( -- * Common Types
    Filter (..),
    FilterFileType (..),
    ChoiceCombo (..),
    ChoiceComboOption (..),
    ChoiceComboSelection (..),

    -- * Open File
    OpenFileOptions (..),
    OpenFileResults (..),
    openFile,

    -- * Save File
    SaveFileOptions (..),
    SaveFileResults (..),
    saveFile,
  )
where

import Control.Exception (throwIO)
import DBus (InterfaceName, Variant)
import DBus qualified
import DBus.Client qualified as DBus
import Data.Default.Class (Default (def))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import Desktop.Portal.Request.Internal (Request, sendRequest)
import Desktop.Portal.Util (encodeNullTerminatedUtf8, mapJust, optionalFromVariant, toVariantPair, toVariantPair')

data Filter = Filter
  { name :: Text,
    fileTypes :: [FilterFileType]
  }
  deriving (Eq, Show)

data FilterFileType
  = GlobFilter Text
  | MimeFilter Text
  deriving (Eq, Show)

data ChoiceCombo = ChoiceCombo
  { id :: Text,
    label_ :: Text,
    choices :: [ChoiceComboOption],
    defaultChoiceId :: Text
  }
  deriving (Eq, Show)

data ChoiceComboOption = ChoiceComboOption
  { id :: Text,
    label_ :: Text
  }
  deriving (Eq, Show)

data ChoiceComboSelection = ChoiceComboSelection
  { comboId :: Text,
    optionId :: Text
  }
  deriving (Eq, Show)

data OpenFileOptions = OpenFileOptions
  { parentWindow :: Maybe Text,
    title :: Maybe Text,
    acceptLabel :: Maybe Text,
    modal :: Maybe Bool,
    multiple :: Maybe Bool,
    directory :: Maybe Bool,
    filters :: Maybe [Filter],
    currentFilter :: Maybe Filter,
    choices :: Maybe [ChoiceCombo]
  }
  deriving (Eq, Show)

instance Default OpenFileOptions where
  def =
    OpenFileOptions
      { parentWindow = Nothing,
        title = Nothing,
        acceptLabel = Nothing,
        modal = Nothing,
        multiple = Nothing,
        directory = Nothing,
        filters = Nothing,
        currentFilter = Nothing,
        choices = Nothing
      }

data OpenFileResults = OpenFileResults
  { uris :: [Text],
    choices :: Maybe [ChoiceComboSelection],
    currentFilter :: Maybe Filter
  }
  deriving (Eq, Show)

data SaveFileOptions = SaveFileOptions
  { parentWindow :: Maybe Text,
    title :: Maybe Text,
    acceptLabel :: Maybe Text,
    modal :: Maybe Bool,
    filters :: Maybe [Filter],
    currentFilter :: Maybe Filter,
    choices :: Maybe [ChoiceCombo],
    currentName :: Maybe Text,
    currentFolder :: Maybe Text,
    currentFile :: Maybe Text
  }
  deriving (Eq, Show)

instance Default SaveFileOptions where
  def =
    SaveFileOptions
      { parentWindow = Nothing,
        title = Nothing,
        acceptLabel = Nothing,
        modal = Nothing,
        filters = Nothing,
        currentFilter = Nothing,
        choices = Nothing,
        currentName = Nothing,
        currentFolder = Nothing,
        currentFile = Nothing
      }

data SaveFileResults = SaveFileResults
  { uris :: [Text],
    choices :: Maybe [ChoiceComboSelection],
    currentFilter :: Maybe Filter
  }
  deriving (Eq, Show)

fileChooserInterface :: InterfaceName
fileChooserInterface = "org.freedesktop.portal.FileChooser"

openFile :: OpenFileOptions -> IO (Request OpenFileResults)
openFile options =
  sendRequest fileChooserInterface "OpenFile" args optionsArg parseOpenFileResponse
  where
    args = [DBus.toVariant parentWindow, DBus.toVariant title]
    parentWindow = fromMaybe "" options.parentWindow
    title = fromMaybe "" options.title
    optionsArg =
      Map.fromList . catMaybes $
        [ toVariantPair "accept_label" options.acceptLabel,
          toVariantPair "modal" options.modal,
          toVariantPair "multiple" options.multiple,
          toVariantPair "directory" options.directory,
          toVariantPair' (fmap encodeFilter) "filters" options.filters,
          toVariantPair' encodeFilter "current_filter" options.currentFilter,
          toVariantPair' (fmap encodeCombo) "choices" options.choices
        ]

saveFile :: SaveFileOptions -> IO (Request SaveFileResults)
saveFile options =
  sendRequest fileChooserInterface "SaveFile" args optionsArgs parseResponse
  where
    args = [DBus.toVariant parentWindow, DBus.toVariant title]
    parentWindow = fromMaybe "" options.parentWindow
    title = fromMaybe "" options.title
    optionsArgs =
      Map.fromList . catMaybes $
        [ toVariantPair "accept_label" options.acceptLabel,
          toVariantPair "modal" options.modal,
          toVariantPair' (fmap encodeFilter) "filters" options.filters,
          toVariantPair' encodeFilter "current_filter" options.currentFilter,
          toVariantPair' (fmap encodeCombo) "choices" options.choices,
          toVariantPair "current_name" options.currentName,
          toVariantPair "current_folder" (encodeNullTerminatedUtf8 <$> options.currentFolder),
          toVariantPair "current_file" (encodeNullTerminatedUtf8 <$> options.currentFile)
        ]

    parseResponse resMap = do
      OpenFileResults {uris, choices, currentFilter} <- parseOpenFileResponse resMap
      pure SaveFileResults {uris, choices, currentFilter}

parseOpenFileResponse :: Map Text Variant -> IO OpenFileResults
parseOpenFileResponse = \case
  resMap
    | Just uris <- DBus.fromVariant =<< Map.lookup "uris" resMap,
      Just choicesRaw <- optionalFromVariant "choices" resMap,
      choices <- fmap decodeChoiceComboSelection <$> choicesRaw,
      Just currentFilterRaw <- optionalFromVariant "current_filter" resMap,
      Just currentFilter <- mapJust decodeFilter currentFilterRaw ->
        pure OpenFileResults {uris, choices, currentFilter}
  resMap ->
    throwIO . DBus.clientError $ "openFile: could not parse response: " <> show resMap

encodeFilter :: Filter -> (Text, [(Word32, Text)])
encodeFilter filtr =
  (filtr.name, encodeFilterFileType <$> filtr.fileTypes)

decodeFilter :: (Text, [(Word32, Text)]) -> Maybe Filter
decodeFilter (name, rawFileTypes) = do
  fileTypes <- traverse decodeFileType rawFileTypes
  pure Filter {name, fileTypes}

encodeFilterFileType :: FilterFileType -> (Word32, Text)
encodeFilterFileType = \case
  GlobFilter pat -> (0, pat)
  MimeFilter mime -> (1, mime)

decodeFileType :: (Word32, Text) -> Maybe FilterFileType
decodeFileType = \case
  (0, pat) -> Just (GlobFilter pat)
  (1, mime) -> Just (MimeFilter mime)
  _ -> Nothing

encodeCombo :: ChoiceCombo -> (Text, Text, [(Text, Text)], Text)
encodeCombo combo =
  ( combo.id,
    combo.label_,
    encodeComboOption <$> combo.choices,
    combo.defaultChoiceId
  )

encodeComboOption :: ChoiceComboOption -> (Text, Text)
encodeComboOption option =
  (option.id, option.label_)

decodeChoiceComboSelection :: (Text, Text) -> ChoiceComboSelection
decodeChoiceComboSelection =
  uncurry ChoiceComboSelection
