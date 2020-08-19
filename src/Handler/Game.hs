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
import           Controller.Game                ( isPlayerAllowedToPlay
                                                , updateStatusLastRound
                                                , giveHint
                                                , playCard
                                                , discardCard
                                                )
import           Data.List.Split                ( chunksOf )
import           Data.ByteString.Lazy           ( fromStrict
                                                , ByteString
                                                )
import qualified Data.Text                     as T
import           Data.Aeson                     ( eitherDecode )
import           Data.List                      ( find )

getGameStatus :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
getGameStatus gameid = do
  eResult <- getGameFromDB gameid >>= filterOwnUser
  case eResult of
    (Left  error) -> json $ errorJson 10 (error :: String)
    (Right game ) -> json $ sucessJson sucessCode game

filterOwnUser
  :: (ListContains n User xs)
  => (Either String Game)
  -> AppHandle (HVect xs) (Either String Game)
filterOwnUser (Left  error) = return $ Left error
filterOwnUser (Right game ) = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  let (Key _uid)   = uid user
  let onlyOtherPlayers =
        [ player | player <- (players game), _uid /= (playerId player) ]
  let newGame = game { players = onlyOtherPlayers }
  return (Right newGame)


makeMove :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
makeMove gameId = do
  rawBodyStr <- fromStrict <$> body
  eGame      <- getGameFromDB gameId
  oldCtx     <- getContext
  let user :: User = (findFirst oldCtx)
  let (Key _uid)   = uid user
  _eGame <-
    (generalMoveCheck _uid eGame (parseMoveBody rawBodyStr))
    >>= (specificMoveCheck _uid eGame)
    >>= (updateGameStatus eGame)
    >>= filterOwnUser
  case _eGame of
    (Left  error) -> json $ errorJson 11 error
    (Right game ) -> json $ sucessJson sucessCode game
 where
  parseMoveBody :: ByteString -> Either String MoveAction
  parseMoveBody jsonByteString = eitherDecode jsonByteString

  generalMoveCheck
    :: String
    -> Either String Game
    -> Either String MoveAction
    -> AppHandle (HVect xs) (Either String (MoveAction))
  generalMoveCheck _    (Left error) _                  = return (Left error)
  generalMoveCheck _    _            (Left  error     ) = return (Left error)
  generalMoveCheck _uid (Right game) (Right moveAction) = do
    if isPlayerAllowedToPlay _uid game
      then do
        if (state game) == Won || (state game) == Lost
          then return (Left "Game already ended!")
          else return (Right (moveAction))
      else return (Left "It's not your turn!")

  specificMoveCheck
    :: String
    -> Either String Game
    -> Either String MoveAction
    -> AppHandle (HVect xs) (Either String (MoveAction))
  specificMoveCheck _ (Left error) _            = return (Left error)
  specificMoveCheck _ _            (Left error) = return (Left error)
  specificMoveCheck _uid (Right game) (Right HintAction { targetPlayer = _targetPId, hint = eHint })
    = (isTargetPlayerAvailable
        game
        (HintAction { targetPlayer = _targetPId, hint = eHint })
      )
      >>= doesPlayerHintHimself _uid
      >>= isRainbowHint
  specificMoveCheck _uid (Right game) (Right PlayAction { cardId = _cardId }) =
    do
      mError <- canFindPlayer _uid game >>= cardInPlayersHand "playable" _cardId
      case mError of
        Nothing      -> return (Right PlayAction { cardId = _cardId })
        (Just error) -> return (Left error)
  specificMoveCheck _uid (Right game) (Right DiscardAction { cardId = _cardId })
    = do
      mError <-
        canFindPlayer _uid game >>= cardInPlayersHand "discardable" _cardId
      case mError of
        Nothing      -> return (Right DiscardAction { cardId = _cardId })
        (Just error) -> return (Left error)

  cardInPlayersHand _ _ (Left error) = return (Just error)
  cardInPlayersHand errorString _cardId (Right _player) =
    if find (\card -> _cardId == card) (cards _player) /= Nothing
      then return Nothing
      else return (Just ("Card not " ++ errorString ++ "!"))

  canFindPlayer _uid game =
    let _mPlayer = find (\player -> _uid == (playerId player)) (players game)
    in  case _mPlayer of
          Nothing      -> return (Left "You are not part of the game!")
          Just _player -> return (Right _player)

  isRainbowHint (Left error) = return (Left error)
  isRainbowHint (Right HintAction { targetPlayer = _targetPId, hint = eHint })
    = do
      case eHint of
        (Right number) ->
          return (Right HintAction { targetPlayer = _targetPId, hint = eHint })
        (Left color) -> do
          if color == Rainbow
            then return (Left "You cannot give Rainbow hints!")
            else return
              (Right HintAction { targetPlayer = _targetPId, hint = eHint })

  doesPlayerHintHimself _ (Left error) = return (Left error)
  doesPlayerHintHimself _uid (Right HintAction { targetPlayer = _targetPId, hint = eHint })
    = do
      if _targetPId == _uid
        then return (Left "You cannot hint yourself!")
        else return
          (Right (HintAction { targetPlayer = _targetPId, hint = eHint }))

  isTargetPlayerAvailable game (HintAction { targetPlayer = _targetPId, hint = eHint })
    = do
      if find (\player -> _targetPId == (playerId player)) (players game)
         /= Nothing
      then
        return (Right HintAction { targetPlayer = _targetPId, hint = eHint })
      else
        return (Left "Target-Player does not exist!")

  updateGameStatus
    :: Either String Game
    -> Either String MoveAction
    -> AppHandle (HVect xs) (Either String Game)
  updateGameStatus (Left error) _                  = return (Left error)
  updateGameStatus _            (Left  error     ) = return (Left error)
  updateGameStatus (Right game) (Right moveAction) = do
    eUpdatedGame <- _updateGameStatus (updateStatusLastRound game) moveAction
    case eUpdatedGame of
      (Left  error      ) -> return (Left error)
      (Right updatedGame) -> do
        updateObject updatedGame
        return (Right updatedGame)
   where
    _updateGameStatus
      :: Game -> MoveAction -> AppHandle (HVect xs) (Either String Game)
    _updateGameStatus game (HintAction { targetPlayer = _targetP, hint = _hint })
      = giveHint _hint _targetP game
    _updateGameStatus game (PlayAction { cardId = _cid }) =
      playCard (currentPlayer game) _cid game
    _updateGameStatus game (DiscardAction { cardId = _cid }) =
      discardCard (currentPlayer game) _cid game

getCards:: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
getCards gameId = do
  eGame      <- getGameFromDB gameId
  _eGame <- filterOwnUser eGame
  case _eGame of
    (Left  error) -> json $ errorJson 12 (error :: String)
    (Right game ) -> do
      let handCards = [ ((playerId player), (cards player)) | player <- players game ]
      let tupleDiscardPile = ("discardPile", (discardPile game))
      let cardIdsToFind = tupleDiscardPile : handCards
      cardsToDisplay <- forM cardIdsToFind (\(string, list)-> do 
        (mCards :: [Maybe Card]) <- forM list findById
        let cards = [card | (Just card) <- mCards]
        return (string, cards)
        )
      json $ sucessJson sucessCode cardsToDisplay

getGameFromDB :: String -> AppHandle (HVect xs) (Either String Game)
getGameFromDB gameId = do
  (mGame :: Maybe Game) <- findById gameId
  case mGame of
    Nothing     -> return (Left ("No game found with Id: " ++ gameId))
    (Just game) -> return (Right game)
