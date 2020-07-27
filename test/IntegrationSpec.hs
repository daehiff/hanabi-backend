{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec where

import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai                    ( Middleware )

import           Utils                          ( flushDB )
import           Model.Utils                    ( setupDB )
import           Init                           ( app
                                                , createConfig
                                                )
import           Web.Spock                      ( spock
                                                , spockAsApp
                                                )
import           Web.Spock.Config
import           Database.MongoDB               ( Pipe )
import           Responses
----------------------------------------------------------------
import           Integration.Auth               ( authTest )
import           Integration.Lobby              ( lobbyTest )

beforeAll = do
  flushDB

afterAll = do
  flushDB


testApp :: IO Middleware
testApp = do
  appcfg   <- createConfig
  pool     <- setupDB $ dbConf appcfg
  spockCfg <- defaultSpockCfg () (PCPool pool) appcfg
  spock spockCfg app

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (spockAsApp testApp) $ do
  authTest
  lobbyTest

