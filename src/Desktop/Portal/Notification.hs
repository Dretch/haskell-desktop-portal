module Desktop.Portal.Notification
  ( -- * Add Notification
    AddNotificationOptions (..),
    NotificationPriority (..),
    NotificationIcon (..),
    NotificationButton (..),
    addNotificationOptions,
    addNotification,

    -- * Remove Notification
    RemoveNotificationOptions (..),
    removeNotification,

    -- * Signals
    NotificationActionInvokedCallback,
    handleNotificationActionInvoked,
  )
where

import Control.Exception (throwIO)
import DBus (InterfaceName, Variant)
import DBus qualified
import DBus.Client qualified as DBus
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Desktop.Portal.Internal (Client, SignalHandler, handleSignal, sendRequestSansResponse)
import Desktop.Portal.Util (toVariantPair, toVariantPair')
import Prelude hiding (id)

data AddNotificationOptions = AddNotificationOptions
  { id :: Text,
    title :: Maybe Text,
    body :: Maybe Text,
    priority :: Maybe NotificationPriority,
    icon :: Maybe NotificationIcon,
    defaultAction :: Maybe Text,
    defaultActionTarget :: Maybe Variant,
    buttons :: Maybe [NotificationButton]
  }
  deriving (Eq, Show)

data NotificationPriority
  = NotificationPriorityLow
  | NotificationPriorityNormal
  | NotificationPriorityHigh
  | NotificationPriorityUrgent
  deriving (Eq, Show)

data NotificationIcon
  = NotificationIconThemed [Text]
  | NotificationIconBytes ByteString
  deriving (Eq, Show)

data NotificationButton = NotificationButton
  { label_ :: Text,
    action :: Text,
    target :: Maybe Variant
  }
  deriving (Eq, Show)

addNotificationOptions ::
  -- | The id of the notification
  Text ->
  AddNotificationOptions
addNotificationOptions id =
  AddNotificationOptions
    { id,
      title = Nothing,
      body = Nothing,
      priority = Nothing,
      icon = Nothing,
      defaultAction = Nothing,
      defaultActionTarget = Nothing,
      buttons = Nothing
    }

newtype RemoveNotificationOptions = RemoveNotificationOptions
  {id :: Text}
  deriving (Eq, Show)

notificationInterface :: InterfaceName
notificationInterface = "org.freedesktop.portal.Notification"

addNotification :: Client -> AddNotificationOptions -> IO ()
addNotification client options =
  sendRequestSansResponse client notificationInterface "AddNotification" [id, optionsArg]
  where
    id = DBus.toVariant options.id
    optionsArg =
      DBus.toVariant . Map.fromList . catMaybes $
        [ toVariantPair "title" options.title,
          toVariantPair "body" options.body,
          toVariantPair' encodePriority "priority" options.priority,
          toVariantPair' encodeIcon "icon" options.icon,
          toVariantPair "default-action" options.defaultAction,
          ("default-action-target",) <$> options.defaultActionTarget,
          toVariantPair' (fmap encodeButton) "buttons" options.buttons
        ]

removeNotification :: Client -> RemoveNotificationOptions -> IO ()
removeNotification client options =
  sendRequestSansResponse client notificationInterface "RemoveNotification" [id]
  where
    id = DBus.toVariant options.id

type NotificationActionInvokedCallback =
  -- | The id of the notification that was clicked.
  Text ->
  -- | The name of the action that was invoked.
  Text ->
  -- | The target parameter that goes along with the action, if any.
  Maybe Variant ->
  -- | A command to run when the action is invoked.
  IO ()

-- | Listen for notification actions being invoked.
handleNotificationActionInvoked :: Client -> NotificationActionInvokedCallback -> IO SignalHandler
handleNotificationActionInvoked client handler =
  handleSignal client notificationInterface "ActionInvoked" $ \signalBody -> do
    case signalBody of
      [notificationId, actionName, parameter]
        | Just notificationId' <- DBus.fromVariant notificationId,
          Just actionName' <- DBus.fromVariant actionName,
          Just parameter' <- DBus.fromVariant parameter -> do
            handler notificationId' actionName' (listToMaybe parameter')
      _ ->
        throwIO . DBus.clientError $ "handleNotificationActionInvoked: could not parse signal body: " <> show signalBody

encodePriority :: NotificationPriority -> Text
encodePriority = \case
  NotificationPriorityLow -> "low"
  NotificationPriorityNormal -> "normal"
  NotificationPriorityHigh -> "high"
  NotificationPriorityUrgent -> "urgent"

encodeIcon :: NotificationIcon -> (Text, Variant)
encodeIcon = \case
  NotificationIconThemed iconNames -> ("themed", DBus.toVariant iconNames)
  NotificationIconBytes bytes -> ("bytes", DBus.toVariant bytes)

encodeButton :: NotificationButton -> Map Text Variant
encodeButton button =
  Map.fromList . catMaybes $
    [ toVariantPair "label" (Just button.label_),
      toVariantPair "action" (Just button.action),
      toVariantPair "target" button.target
    ]
