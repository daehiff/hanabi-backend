module Main where

import           Init                           ( runApp )

import           Web.Spock
import           Web.Spock.Config
import Network.Wai.Middleware.Static

import           Control.Monad.Trans
import           Database.MongoDB hiding(lookup)
import Network.HTTP.Types.URI (parseQueryText)
import Data.List (dropWhileEnd, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text                     as T



main :: IO ()
main = runApp
