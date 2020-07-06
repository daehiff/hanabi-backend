module Main where

import           Init (runApp)

import qualified Data.Bson                     as BSON

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(parseJSON)
                                                , decode
                                                , encode
                                                )
import           Data.Time.Clock                
import Data.Time.Calendar
main :: IO ()
main = runApp