module Desktop.Portal.NotificationSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Monad (void)
import DBus (InterfaceName, toVariant)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word (Word32)
import Desktop.Portal (AddNotificationOptions (..), NotificationButton (..), NotificationIcon (..), NotificationPriority (..), RemoveNotificationOptions (..))
import Desktop.Portal qualified as Portal
import Desktop.Portal.TestUtil
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Prelude hiding (id)

notificationInterface :: InterfaceName
notificationInterface = "org.freedesktop.portal.Notification"

spec :: Spec
spec = do
  around withTestBus $ do
    describe "addNotification" $ do
      it "should encode request with all Nothings" $ \handle -> do
        body <- savingRequestArguments_ handle notificationInterface "AddNotification" False $ do
          void (Portal.addNotification (client handle) (Portal.addNotificationOptions "id"))
        body
          `shouldBe` [ toVariantText "id",
                       toVariantMap []
                     ]

      it "should encode request with all Justs" $ \handle -> do
        body <- savingRequestArguments_ handle notificationInterface "AddNotification" False $ do
          let options =
                AddNotificationOptions
                  { id = "id",
                    title = Just "title",
                    body = Just "body",
                    priority = Just NotificationPriorityLow,
                    icon = Just (NotificationIconThemed ["wave"]),
                    defaultAction = Just "defaultAction",
                    defaultActionTarget = Just (toVariant (42 :: Word32)),
                    buttons = Just [NotificationButton {label_ = "buttonLabel", action = "buttonAction", target = Nothing}]
                  }
          void (Portal.addNotification (client handle) options)
        body
          `shouldBe` [ toVariantText "id",
                       toVariantMap
                         [ ("title", toVariantText "title"),
                           ("body", toVariantText "body"),
                           ("priority", toVariantText "low"),
                           ("icon", toVariant ("themed" :: Text, toVariant ["wave" :: Text])),
                           ("default-action", toVariantText "defaultAction"),
                           ("default-action-target", toVariant (42 :: Word32)),
                           ( "buttons",
                             toVariant
                               [ Map.fromList
                                   [ ("label" :: Text, toVariantText "buttonLabel"),
                                     ("action", toVariantText "buttonAction")
                                   ]
                               ]
                           )
                         ]
                     ]

    describe "removeNotification" $ do
      it "should encode request" $ \handle -> do
        body <- savingRequestArguments_ handle notificationInterface "RemoveNotification" False $ do
          void (Portal.removeNotification (client handle) (RemoveNotificationOptions "id"))
        body
          `shouldBe` [toVariantText "id"]

    describe "handleNotificationActionInvoked" $ do
      it "should decode action invoked signal" $ \handle -> do
        decodedSignalVar <- newEmptyMVar
        void $
          Portal.handleNotificationActionInvoked
            (client handle)
            (\id name param -> putMVar decodedSignalVar (id, name, param))
        let signalArgs = [toVariantText "id", toVariantText "action", toVariant [toVariant (42 :: Word32)]]
        sendSignal handle notificationInterface "ActionInvoked" signalArgs
        decoded <- readMVar decodedSignalVar
        decoded `shouldBe` ("id", "action", Just (toVariant (42 :: Word32)))
