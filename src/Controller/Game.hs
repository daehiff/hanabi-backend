{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Controller.Game where -- no control

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

createDrawPile :: Bool -> IO [Card]
createDrawPile isRainbow = do
  let colors =
        (if isRainbow
          then [Red, Green, Blue, White, Yellow, Rainbow]
          else [Red, Green, Blue, White, Yellow]
        )
  let numbers = [1, 1, 1, 2, 2, 3, 3, 4, 4, 5]
  let cards =
        [ Card { cid = NewKey, color = color, number = number }
        | color  <- colors
        , number <- numbers
        ]
  shuffle cards


distributeCards :: [Player] -> [String] -> ([Player], [String])
distributeCards players cardIDs = do
  let amtCards = if length (players) < 4 then 5 else 4
      restCards = drop (amtCards * length (players)) cardIDs
      cardsToDistribute = chunksOf amtCards $ take (amtCards *length(players)) cardIDs
      newPlayers = zipWith (\player cards -> player{cards = cards}) players cardsToDistribute
      in  (newPlayers, restCards)
  


createGame :: [String] -> Settings -> AppHandle (HVect xs) Game
createGame users settings = do
  drawPile <- (liftIO $ createDrawPile (isRainbow settings))
    >>= \pile -> forM pile insertObject
  let drawPileIDs = [ _cid | (Key _cid) <- map cid drawPile ]
  let players = [ Player { pid                 = NewKey
             , correspondingUserID = _uid
             , cards               = []
             , explicitHints       = []
             }| _uid <- users]
  let (newPlayers, restCards) = distributeCards players drawPileIDs
  players <- forM newPlayers insertObject
  let userPlayers = map
        (\player ->
          let (Key _id) = pid player in (correspondingUserID player, _id)
        )
        players
  let game = Game { gid           = NewKey
                  , currentPlayer = userPlayers !! 0
                  , players       = userPlayers
                  , hints         = amtHints settings
                  , lives         = amtLives settings
                  , drawPile      = restCards
                  , discardPile   = []
                  , redPile       = 0
                  , greenPile     = 0
                  , bluePile      = 0
                  , yellowPile    = 0
                  , whitePile     = 0
                  , rainbowPile   = 0
                  }
  insertObject game
