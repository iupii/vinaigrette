{-# LANGUAGE OverloadedStrings #-}

module Specs where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent as C
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Client
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Vinaigrette.App
import Vinaigrette.Api
import Vinaigrette.Server
import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
    specs <- test_specTests
    defaultMain specs

test_specTests :: IO TestTree
test_specTests = testGroup "Specs" <$> sequence [
      testSpec "trivial" trivialSpec
    , testSpec "endpoint" endpointSpec
    ]

trivialSpec :: Spec
trivialSpec = parallel $ do
    it "is trivially true" $ do
        True `shouldBe` True

withApplication :: IO () -> IO ()
withApplication action = do
    appLogger <- newStdoutLoggerSet defaultBufSize
    let env = Env appLogger
    bracket (liftIO $ C.forkIO $ Warp.run 8888 (application env))
        C.killThread
        (const action)

endpointSpec :: Spec
endpointSpec =
    around_ withApplication $ do
        let client' = client (Proxy :: Proxy Api)
        baseUrl' <- runIO $ parseBaseUrl "http://localhost:8888"
        manager' <- runIO $ newManager defaultManagerSettings
        let clientEnv = mkClientEnv manager' baseUrl'
        -- testing scenarios start here
        describe "GET /" $ do
            it "should get \"Hello World\"" $ do
                result <- runClientM client' clientEnv
                result `shouldBe` (Right "Hello World")
