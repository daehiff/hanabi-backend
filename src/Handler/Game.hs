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
    eResult <- getGameFromDB gameid >>= filterOwnUser
    case eResult of 
      (Left error) -> json $ errorJson 10 (error :: String)
      (Right game) -> json $ sucessJson sucessCode game
  where  
    filterOwnUser:: (ListContains n User xs) => (Either String Game) -> AppHandle (HVect xs) (Either String Game)
    filterOwnUser (Left error) = return $ Left error
    filterOwnUser (Right game) = do
        oldCtx <- getContext
        let user :: User = (findFirst oldCtx)
        let (Key _uid) = uid user
        let onlyOtherPlayers = [player | player <- (players game), _uid /= (correspondingUserID player)]
        let newGame = game {players = onlyOtherPlayers}
        return (Right newGame)
    getGameFromDB:: (ListContains n User xs) => String -> AppHandle (HVect xs) (Either String Game)
    getGameFromDB gameId = do 
        (mGame :: Maybe Game) <- findById gameId
        case mGame of 
          Nothing -> return (Left ("No game found with Id: " ++ gameId))
          (Just game) -> return (Right game)


