module Desktop.Portal.Util
  ( optionalFromVariant,
    mapJust,
    toVariantPair,
    toVariantPair',
    encodeNullTerminatedUtf8,
    decodeNullTerminatedUtf8,
    decodeFileUri,
    decodeFileUris,
  )
where

import DBus (IsVariant, Variant)
import DBus qualified
import Data.Binary.Builder qualified as Binary
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as Bytes
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, unpack)
import Data.Text.Encoding qualified as Encoding
import Text.URI (Authority (..), URI (..))
import Text.URI qualified as URI

-- | Returns @Just Nothing@ if the field does not exist, @Just (Just x)@ if it does exist and
-- can be turned into the expected type, or @Nothing@ if the field exists with the wrong type.
optionalFromVariant :: forall a. (IsVariant a) => Text -> Map Text Variant -> Maybe (Maybe a)
optionalFromVariant key variants =
  mapJust DBus.fromVariant (Map.lookup key variants)

mapJust :: (a -> Maybe b) -> Maybe a -> Maybe (Maybe b)
mapJust f = \case
  Nothing -> Just Nothing
  Just x -> Just <$> f x

toVariantPair :: (IsVariant a) => Text -> Maybe a -> Maybe (Text, Variant)
toVariantPair = toVariantPair' id

toVariantPair' :: (IsVariant b) => (a -> b) -> Text -> Maybe a -> Maybe (Text, Variant)
toVariantPair' f key = \case
  Nothing -> Nothing
  Just x -> Just (key, DBus.toVariant (f x))

encodeNullTerminatedUtf8 :: Text -> ByteString
encodeNullTerminatedUtf8 txt =
  Binary.toLazyByteString (Encoding.encodeUtf8Builder txt <> Binary.singleton 0)

decodeNullTerminatedUtf8 :: ByteString -> Maybe Text
decodeNullTerminatedUtf8 bytes =
  case Encoding.decodeUtf8' (Bytes.toStrict (Bytes.dropWhileEnd (== 0) bytes)) of
    Left _err -> Nothing
    Right t -> Just t

decodeFileUri :: Text -> Maybe FilePath
decodeFileUri uri =
  case URI.mkURI uri of
    Just
      URI
        { uriScheme = Just (URI.unRText -> "file"),
          uriAuthority = (validAuthority -> True),
          uriPath = Just (_trailingSlash, parts),
          uriQuery = [],
          uriFragment = Nothing
        } -> Just . unpack $ foldMap (("/" <>) . URI.unRText) parts
    _ ->
      Nothing
  where
    validAuthority = \case
      Left True -> True
      Right
        Authority
          { authUserInfo = Nothing,
            authHost = (URI.unRText -> ""),
            authPort = Nothing
          } -> True
      _ -> False

decodeFileUris :: [Text] -> Maybe [FilePath]
decodeFileUris = traverse decodeFileUri
