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


createGame :: [String] -> Settings -> AppHandle (HVect xs) Game
createGame players settings = do
  drawPile <- (liftIO $ createDrawPile (isRainbow settings)) >>= \pile -> forM pile insertObject
  let drawPileIDs = [_cid | (Key _cid) <- map cid drawPile]
  let game = Game { gid           = NewKey
                  , currentPlayer = players !! 0
                  , players       = players
                  , hints         = amtHints settings
                  , lives         = amtLives settings
                  , drawPile      = drawPileIDs
                  , discardPile   = []
                  , redPile       = 0
                  , greenPile     = 0
                  , bluePile      = 0
                  , yellowPile    = 0
                  , whitePile     = 0
                  , rainbowPile   = 0
                  }
  insertObject game
