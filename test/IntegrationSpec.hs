{-# LANGUAGE OverloadedStrings #-}

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
import           Integration.AuthTest           ( authTest )
import           Integration.Lobby              ( lobbyTest )
import           Utils                          ( testApp )

beforeAll = do
  flushDB

afterAll = do
  flushDB


main :: IO ()
main = hspec spec

spec :: Spec
spec = with (spockAsApp testApp) $ do
  authTest
  lobbyTest

