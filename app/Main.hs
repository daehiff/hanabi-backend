module Main where

import           Init (runApp)

import qualified Data.Bson                     as BSON

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(parseJSON)
                                                , decode
                                                , encode
                                                )

main :: IO ()
main = runApp