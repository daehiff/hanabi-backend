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
        , maxPoints = 5 * (length $ getColorsForGame (isRainbow settings)) -- 5 because it's the highest number per pile you can get
        , lastPlayerToMove = Nothing
        , settings         = settings
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
      then card { hintColor = removeDuplicates (hcolor:(hintColor card)) }
      else card
  updateCard (Right hnumber) card = if (number card) == hnumber
    then card { hintNumber = Just hnumber }
    else card
  removeDuplicates :: Eq a => [a] -> [a]
  removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs


playCard
  :: String -> String -> Game -> AppHandle (HVect xs) (Either String Game)
playCard _pid _cid game = do
  ((mCardToPlay) :: Maybe Card) <- findById _cid
  let (Just cardToPlay) = mCardToPlay
  let (Just player) =
        find (\_player -> _pid == (playerId _player)) (players game)
  let newPlayer = removeCardFromPlayer player _cid
  if doesPlayedCardFit game cardToPlay
    then do
      adjGame <- playedValidCard game cardToPlay
      let handCards = concat [ cards player | player <- players adjGame ]
      let playableCardIDs = (drawPile adjGame) ++ handCards
      (mPlayableCards :: [Maybe Card]) <- forM playableCardIDs findById
      let playableCards = [ card | (Just card) <- mPlayableCards ]
      if didWin adjGame || isGameOver adjGame playableCards
        then return (Right (adjGame { state = Won }))
        else continueGame adjGame newPlayer
    else do
      adjGame <- playedInvalidCard game _cid
      if didLoose adjGame
        then return (Right (adjGame { state = Lost }))
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
    if (number card) == 5
      then return game { piles = newPiles, points = points game + 1, hints = min (amtHints (settings game)) ((hints game)+1)}
      else return game { piles = newPiles, points = points game + 1 }

  didWin :: Game -> Bool
  didWin game = (points game) == (maxPoints game)

  didLoose :: Game -> Bool
  didLoose game = (lives game) == 0

  continueGame :: Game -> Player -> AppHandle (HVect xs) (Either String Game)
  continueGame game _player = do
    let (newPlayer, adjGame) = drawNewCard game _player
    let _adjGame =
          adjGame { players = updatePlayer (players adjGame) newPlayer }
    if shouldStartLastRound _adjGame
      then return
        (Right
          (setNextPlayer
            (_adjGame { state            = LastRound
                      , lastPlayerToMove = Just (currentPlayer _adjGame)
                      }
            )
          )
        )
      else return (Right (setNextPlayer _adjGame))

discardCard
  :: String -> String -> Game -> AppHandle (HVect xs) (Either String Game)
discardCard _pid _cid game = do
  if (hints game) == amtHints (settings game)
    then return (Left "Cannot discard, because max amount of hints is reached!")
    else do
      ((mCardToPlay) :: Maybe Card) <- findById _cid
      let (Just cardToPlay) = mCardToPlay

      let (Just player) =
            find (\_player -> _pid == (playerId _player)) (players game)

      let newPlayer       = removeCardFromPlayer player _cid
      let discardedGame   = game { discardPile = _cid : (discardPile game) }

      let handCards = concat [ cards player | player <- players discardedGame ]
      let playableCardIDs = (drawPile discardedGame) ++ handCards
      (mPlayableCards :: [Maybe Card]) <- forM playableCardIDs findById
      let playableCards = [ card | (Just card) <- mPlayableCards ]

      if isGameOver discardedGame playableCards
        then return (Right (discardedGame { state = Won }))
        else do
          let (_player, _game) = drawNewCard discardedGame newPlayer
          let newGame = _game { hints   = hints _game + 1
                              , players = updatePlayer (players _game) _player
                              }
          if shouldStartLastRound newGame
            then return
              (Right
                (setNextPlayer
                  (newGame { state            = LastRound
                           , lastPlayerToMove = Just (currentPlayer newGame)
                           }
                  )
                )
              )
            else return (Right (setNextPlayer newGame))

isGameOver :: Game -> [Card] -> Bool
isGameOver game playableCards =
  let playableCardsXPossibleCards =
          [ ((color card, number card), (_color, _number + 1))
          | (_color, _number) <- piles game
          , card              <- playableCards
          ]
  in  not $ any (\((c1, n1), (c2, n2)) -> c1 == c2 && n1 == n2)
                playableCardsXPossibleCards


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
drawNewCard game player = if (length (drawPile game) == 0)
  then (player, game )
  else
    ( player { cards = ((drawPile game) !! 0) : (cards player) }
    , game { drawPile = drop 1 (drawPile game) }
    )

shouldStartLastRound :: Game -> Bool
shouldStartLastRound game =
  length (drawPile game) == 0 && (state game) == Running

updatePlayer :: [Player] -> Player -> [Player]
updatePlayer players _player = map
  (\_playerObj -> if (playerId _player) == (playerId _playerObj)
    then _player
    else _playerObj
  )
  (players)

updateStatusLastRound :: Game -> Game
updateStatusLastRound game =
  if (state game)
       == LastRound
       && Just (currentPlayer game)
       == (lastPlayerToMove game)
    then game { state = Won }
    else game

isPlayerAllowedToPlay :: String -> Game -> Bool
isPlayerAllowedToPlay _pid _game = _pid == (currentPlayer _game)
