module Desktop.Portal.Account
  ( -- * Get User Information
    GetUserInformationOptions (..),
    GetUserInformationResults (..),
    getUserInformation,
  )
where

import Control.Exception (throwIO)
import DBus (InterfaceName)
import DBus qualified
import DBus.Client qualified as DBus
import Data.Default.Class (Default (def))
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Desktop.Portal.Request.Internal (Client, Request, sendRequest)
import Desktop.Portal.Util (optionalFromVariant, toVariantPair)

data GetUserInformationOptions = GetUserInformationOptions
  { window :: Maybe Text,
    reason :: Maybe Text
  }
  deriving (Eq, Show)

instance Default GetUserInformationOptions where
  def =
    GetUserInformationOptions
      { window = Nothing,
        reason = Nothing
      }

data GetUserInformationResults = GetUserInformationResults
  { id :: Text,
    name :: Text,
    image :: Maybe Text
  }
  deriving (Eq, Show)

accountInterface :: InterfaceName
accountInterface = "org.freedesktop.portal.Account"

getUserInformation :: Client -> GetUserInformationOptions -> IO (Request GetUserInformationResults)
getUserInformation client options =
  sendRequest client accountInterface "GetUserInformation" [window] optionsArg parseResponse
  where
    window = DBus.toVariant (fromMaybe "" options.window)
    optionsArg =
      Map.fromList . catMaybes $
        [ toVariantPair "reason" options.reason
        ]

    parseResponse = \case
      resMap
        | Just id' <- DBus.fromVariant =<< Map.lookup "id" resMap,
          Just name <- DBus.fromVariant =<< Map.lookup "name" resMap,
          Just image <- optionalFromVariant "image" resMap ->
            pure GetUserInformationResults {id = id', name, image}
      resMap ->
        throwIO . DBus.clientError $ "getUserInformation: could not parse response: " <> show resMap
