module Desktop.Portal.SecretSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import DBus (InterfaceName, fromVariant, methodCallBody)
import Desktop.Portal qualified as Portal
import Desktop.Portal.TestUtil
import System.Posix.ByteString (Fd, closeFd, fdWrite)
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Test.Hspec.Expectations (shouldThrow)

secretsInterface :: InterfaceName
secretsInterface = "org.freedesktop.portal.Secret"

spec :: Spec
spec = do
  around withTestBus $ do
    describe "retrieveSecret" $ do
      describe "should read secret from file descriptor" $ do
        it "when secret is written before response is returned" $ \handle -> do
          let answer methodCall = do
                [fromVariant -> Just (fd :: Fd), _opts] <- pure (methodCallBody methodCall)
                void $ fdWrite fd "$secret$"
                closeFd fd
                pure (successResponse [])
          withRequestAnswer handle secretsInterface "RetrieveSecret" answer $ do
            actualSecret <- Portal.retrieveSecret (client handle)
            actualSecret `shouldBe` "$secret$"

        it "when secret is written in multiple chunks" $ \handle -> do
          let answer methodCall = do
                [fromVariant -> Just (fd :: Fd), _opts] <- pure (methodCallBody methodCall)
                void $ fdWrite fd "$sec"
                void $ fdWrite fd "ret$"
                closeFd fd
                pure (successResponse [])
          withRequestAnswer handle secretsInterface "RetrieveSecret" answer $ do
            actualSecret <- Portal.retrieveSecret (client handle)
            actualSecret `shouldBe` "$secret$"

        it "when secret is written after response is returned" $ \handle -> do
          let answer methodCall = do
                [fromVariant -> Just (fd :: Fd), _opts] <- pure (methodCallBody methodCall)
                void $ forkIO $ do
                  threadDelay 100_000
                  void $ fdWrite fd "$secret$"
                  closeFd fd
                pure (successResponse [])
          withRequestAnswer handle secretsInterface "RetrieveSecret" answer $ do
            actualSecret <- Portal.retrieveSecret (client handle)
            actualSecret `shouldBe` "$secret$"

      it "should throw exception on request failure" $ \handle -> do
        let answer _methodCall = do
              pure failureResponse
        withRequestAnswer handle secretsInterface "RetrieveSecret" answer $ do
          Portal.retrieveSecret (client handle)
            `shouldThrow` dbusClientException
