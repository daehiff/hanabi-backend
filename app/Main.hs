module Main where

import           Init                           ( runApp )

import           Web.Spock
import           Web.Spock.Config
import Network.Wai.Middleware.Static

main :: IO ()
main = runApp
