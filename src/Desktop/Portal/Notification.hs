module Desktop.Portal.Notification
  ( -- * Add Notification
    AddNotificationOptions (..),
    NotificationPriority (..),
    NotificationButton (..),
    addNotificationOptions,
    addNotification,

    -- * Remove Notification
    RemoveNotificationOptions (..),
    removeNotification,

    -- * Handle Notification Action

    --  handleNotificationAction,
  )
where

import DBus (InterfaceName, Variant)
import DBus qualified
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Desktop.Portal.Request.Internal (Client, sendRequestSansResponse)
import Desktop.Portal.Util (toVariantPair, toVariantPair')
import Prelude hiding (id)

-- todo: signals? test remove call?

data AddNotificationOptions = AddNotificationOptions
  { id :: Text,
    title :: Maybe Text,
    body :: Maybe Text,
    priority :: Maybe NotificationPriority,
    -- todo: icon
    defaultAction :: Maybe Text,
    defaultActionTarget :: Maybe Variant,
    buttons :: Maybe [NotificationButton]
  }
  deriving (Eq, Show)

data NotificationPriority
  = NotificationLow
  | NotificationNormal
  | NotificationHigh
  | NotificationUrgent
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
          toVariantPair "default-action" options.defaultAction,
          toVariantPair "default-action-target" options.defaultActionTarget,
          toVariantPair' (fmap encodeButton) "buttons" options.buttons
        ]

removeNotification :: Client -> RemoveNotificationOptions -> IO ()
removeNotification client options =
  sendRequestSansResponse client notificationInterface "RemoveNotification" [id]
  where
    id = DBus.toVariant options.id

-- handleNotificationAction ::
--   -- | id of the action to be handled.
--   Text ->
--   IO SignalHandler
-- handleNotificationAction id =
--   DBus.addMatch

encodePriority :: NotificationPriority -> Text
encodePriority = \case
  NotificationLow -> "low"
  NotificationNormal -> "normal"
  NotificationHigh -> "high"
  NotificationUrgent -> "urgent"

encodeButton :: NotificationButton -> Map Text Variant
encodeButton button =
  Map.fromList . catMaybes $
    [ toVariantPair "label" (Just button.label_),
      toVariantPair "action" (Just button.action),
      toVariantPair "target" button.target
    ]
