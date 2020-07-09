module IntegrationSpec where

import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai                    ( Middleware )

import           Utils                          ( flushDB )

import           Init                           ( app )
import           Web.Spock                      ( spock
                                                , spockAsApp
                                                )
import           Web.Spock.Config
----------------------------------------------------------------
import Integration.AuthTest (authTest)

beforeAll = do
  flushDB

afterAll = do
  flushDB

testApp :: IO Middleware
testApp = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  spock spockCfg app

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (spockAsApp testApp) $ do
  authTest