module Main where

import           Init                           ( runApp )


import           Model
import           Model.Utils
import           Controller.Game
import           Data.List                      ( find
                                                , findIndex
                                                )



import Data.Aeson


main :: IO ()
main = runApp


