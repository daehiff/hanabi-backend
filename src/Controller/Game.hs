{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Controller.Game where

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
import           Data.List                      ( find
                                                , findIndex
                                                )

getColorsForGame :: Bool -> [Color]
getColorsForGame isRainbow =
  (if isRainbow
    then [Red, Green, Blue, White, Yellow, Rainbow]
    else [Red, Green, Blue, White, Yellow]
  )

createDrawPile :: Bool -> IO [Card]
createDrawPile isRainbow = do
  let colors  = getColorsForGame isRainbow
  let numbers = [1, 1, 1, 2, 2, 3, 3, 4, 4, 5]
  let cards =
        [ Card { cid        = NewKey
               , color      = color
               , number     = number
               , hintColor  = []
               , hintNumber = Nothing
               }
        | color  <- colors
        , number <- numbers
        ]
  shuffle cards


distributeCards :: [Player] -> [String] -> ([Player], [String])
distributeCards players cardIDs =
  let
    amtCards  = if length (players) < 4 then 5 else 4
    restCards = drop (amtCards * length (players)) cardIDs
    cardsToDistribute =
      chunksOf amtCards $ take (amtCards * length (players)) cardIDs
    newPlayers = zipWith (\player cards -> player { cards = cards })
                         players
                         cardsToDistribute
  in
    (newPlayers, restCards)



createGame :: [User] -> Settings -> AppHandle (HVect xs) Game
createGame users settings = do
  drawPile <- (liftIO $ createDrawPile (isRainbow settings))
    >>= \pile -> forM pile insertObject
  let drawPileIDs = [ _cid | (Key _cid) <- map cid drawPile ]
  let players =
        [ Player { playerId      = let (Key _uid) = uid user in _uid
                 , name          = username user
                 , cards         = []
                 , explicitHints = []
                 }
        | user <- users
        ]
  let (newPlayers, restCards) = distributeCards players drawPileIDs
  let (_uid)                  = playerId $ newPlayers !! 0
  let game = Game
        { gid              = NewKey
        , currentPlayer    = _uid
        , players          = newPlayers
        , hints            = amtHints settings
        , lives            = amtLives settings
        , drawPile         = restCards
        , discardPile      = []
        , piles = [ (pileColor, 0)
                  | pileColor <- getColorsForGame (isRainbow settings)
                  ]
        , state            = Running
        , points           = 0
        , maxPoints = 5 * (length $ getColorsForGame (isRainbow settings))
        , lastPlayerToMove = Nothing
        }
  insertObject game


giveHint
  :: (Either Color Int)
  -> String
  -> Game
  -> AppHandle (HVect xs) (Either String Game)
giveHint hint id game = do
  if (hints game) == 0
    then return (Left "No hints available!")
    else do
      let (Just player) =
            find (\player -> playerId player == id) ((players game))
      (cardObjects :: [Maybe Card]) <- forM (cards player) findById
      let cards         = [ card | (Just card) <- cardObjects ]
      let modifiedCards = map (updateCard hint) cards
      forM modifiedCards updateObject
      let newGame = setNextPlayer game
      return (Right newGame { hints = hints newGame - 1 })
 where
  updateCard :: (Either Color Int) -> Card -> Card
  updateCard (Left hcolor) card =
    if (color card) == hcolor || (color card) == Rainbow
      then card { hintColor = [hcolor] }
      else card
  updateCard (Right hnumber) card = if (number card) == hnumber
    then card { hintNumber = Just hnumber }
    else card


playCard :: String -> String -> Game -> AppHandle (HVect xs) Game
playCard _pid _cid game = do
  ((mCardToPlay) :: Maybe Card) <- findById _cid
  let (Just cardToPlay) = mCardToPlay
  let (Just player) =
        find (\_player -> _pid == (playerId _player)) (players game)
  let newPlayer = removeCardFromPlayer player _cid
  if doesPlayedCardFit game cardToPlay
    then do
      adjGame <- playedValidCard game cardToPlay
      if didWin adjGame
        then return adjGame { state = Won }
        else continueGame adjGame newPlayer
    else do
      adjGame <- playedInvalidCard game _cid
      if didLoose adjGame
        then return adjGame { state = Lost }
        else continueGame adjGame newPlayer
 where
  playedInvalidCard :: Game -> String -> AppHandle (HVect xs) Game
  playedInvalidCard game _cid = do
    let newGame = game { lives       = (lives game) - 1
                       , discardPile = _cid : (discardPile game)
                       }
    return newGame
  playedValidCard :: Game -> Card -> AppHandle (HVect xs) Game
  playedValidCard game card = do
    let newPiles = map
          (\(_color, _number) -> if _color == color card
            then (_color, _number + 1)
            else (_color, _number)
          )
          (piles game)
    return game { piles = newPiles, points = points game + 1 }

  didWin :: Game -> Bool
  didWin game = (points game) == (maxPoints game)

  didLoose :: Game -> Bool
  didLoose game = (lives game) == 0

  continueGame :: Game -> Player -> AppHandle (HVect xs) Game
  continueGame game _player = do
    let (newPlayer, adjGame) = drawNewCard game _player
    let _adjGame = adjGame
          { players = map
                        (\_playerObj ->
                          if (playerId newPlayer) == (playerId _playerObj)
                            then newPlayer
                            else _playerObj
                        )
                        (players adjGame)
          }
    if shouldStartLastRound _adjGame
      then return $ setNextPlayer
        (_adjGame { state            = LastRound
                  , lastPlayerToMove = Just (currentPlayer _adjGame)
                  }
        )
      else return $ setNextPlayer _adjGame

discardCard:: String -> String -> Game -> AppHandle (HVect xs) Game
discardCard _pid _cid game = do
    ((mCardToPlay) :: Maybe Card) <- findById _cid
    let (Just cardToPlay) = mCardToPlay
    let (Just player) =
          find (\_player -> _pid == (playerId _player)) (players game)

    let newPlayer = removeCardFromPlayer player _cid
    let discardedGame = game {discardPile = _cid : (discardPile game)}

    let handCards = concat [cards player | player <- players discardedGame]
    let playableCardIDs = (drawPile discardedGame) ++ handCards
    (mPlayableCards:: [Maybe Card]) <- forM playableCardIDs findById
    let playableCards = [ card | (Just card) <- mPlayableCards]
    if isGameOver discardedGame playableCards
      then return discardedGame{state = Won}
      else do
        let (_player, _game) = drawNewCard discardedGame newPlayer
        let newGame = discardedGame{hints = hints discardedGame + 1}
        if shouldStartLastRound newGame
          then return $ setNextPlayer
            (newGame { state            = LastRound
                      , lastPlayerToMove = Just (currentPlayer newGame)
                      }
            )
          else return $ setNextPlayer newGame
  where
    isGameOver:: Game -> [Card] -> Bool
    isGameOver game playableCards =
      let playableCardsXPossibleCards = [((color card, number card),(_color, _number + 1)) | (_color, _number) <- piles game, card <- playableCards]
      in any (\((c1, n1),(c2, n2)) -> c1 == c2 && n1 == n2) playableCardsXPossibleCards


setNextPlayer :: Game -> Game
setNextPlayer game =
  let playerIndices = [ id | id <- (map playerId (players game)) ]
      (Just index)  = findIndex
        (\playerId -> playerId == (currentPlayer game))
        playerIndices
  in  game
        { currentPlayer = playerIndices
                            !! ((index + 1) `mod` length playerIndices)
        }

doesPlayedCardFit :: Game -> Card -> Bool
doesPlayedCardFit game card =
  let mpile = find (\(_color, _) -> _color == color card) (piles game)
  in  _doesPlayedCardFit mpile card
 where
  _doesPlayedCardFit :: Maybe (Color, Int) -> Card -> Bool
  _doesPlayedCardFit Nothing _ = False
  _doesPlayedCardFit (Just (_color, _number)) card =
    _number + 1 == (number card)


removeCardFromPlayer :: Player -> String -> Player
removeCardFromPlayer player _cid =
  player { cards = [ card | card <- (cards player), card /= _cid ] }

drawNewCard :: Game -> Player -> (Player, Game)
drawNewCard game player =
  ( player { cards = ((drawPile game) !! 0) : (cards player) }
  , game { drawPile = drop 1 (drawPile game) }
  )

shouldStartLastRound :: Game -> Bool
shouldStartLastRound game = length (drawPile game) == 0
