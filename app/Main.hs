module Main where

import           Init                           ( runApp )

import           Web.Spock
import           Web.Spock.Config
import Network.Wai.Middleware.Static
import           Database.MongoDB
import           Text.Read                      ( readMaybe )
import Data.Word (Word32(..))
main :: IO ()
main = runApp
