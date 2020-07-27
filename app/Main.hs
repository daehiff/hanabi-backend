module Main where

import           Init                           ( runApp )


import Model
import Model.Utils


import Data.Aeson (encode)

import           Control.Monad.Trans
import           Database.MongoDB hiding(lookup)
import Network.HTTP.Types.URI (parseQueryText)
import Data.List (dropWhileEnd, lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Text                     as T
import Model.Utils


main :: IO ()
main = runApp


