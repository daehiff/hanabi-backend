{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}

module Handler.Game where -- no control

import           Web.Spock
import           Control.Monad.Trans            ( liftIO )
import           Model
import           Model.Utils
import           System.Random                  ( randomRIO )
import           Responses
import           Web.Spock                      ( ActionCtxT )
import           Data.HVect              hiding ( length
                                                , (!!)
                                                )
import           Controller.Utils               ( shuffle )
import           Control.Monad                  ( forM )
import           Data.List.Split                ( chunksOf )

getGameStatus :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
getGameStatus gameid = do
  (mGame :: Maybe Game) <- findById gameid
  case mGame of 
    Nothing -> json $ errorJson 10 ("No game found" :: String)
    (Just game) -> json $ sucessJson sucessCode game
