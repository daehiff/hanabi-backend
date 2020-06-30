module Main where

import           Init (runApp)

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , encode
                                                , decode)
import qualified Data.Text                     as T
import           Data.HVect
import           Web.Spock
import           Web.Spock.Config
main :: IO ()
main = runApp