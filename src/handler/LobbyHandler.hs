{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LobbyHandler where
import           Web.Spock
import           Data.Text               hiding ( unfoldr
                                                , take
                                                , lines
                                                , length
                                                )
import           Data.List                      ( unfoldr )
import qualified Data.Text                     as T

import           Data.Bson                      ( val
                                                , (=:)
                                                )

import           BSONExtention                  ( ObjectKey(..) )
import           Data.Time.Clock
import           Model                          ( User(..)
                                                , Lobby(..)
                                                )
import           ModelUtils
import           Web.Spock.Config
import           Control.Monad.Trans
import           Data.HVect              hiding ( length
                                                , (!!)
                                                )
import           System.Random                  ( randomRIO )
import           Responses
import           Network.HTTP.Types             ( badRequest400 )
{-
@api POST {{base_url}}/lobby/create create
@apiName create
@apiGroup Lobby
@apiDescription create a new Lobby
-}
createLobby :: (MonadIO m, ListContains n User xs) => ActionCtxT (HVect xs) m b
createLobby = do
  oldCtx <- getContext
  let host :: User = (findFirst oldCtx)
  let (Key _id)    = uid host
  now  <- liftIO getCurrentTime
  salt <- liftIO generateSalt
  let public   = True
  let launched = False
  let lobbyC = Lobby { lid          = NewKey
                     , lobbyHost    = _id
                     , player       = []
                     , kickedPlayer = []
                     , created      = now
                     , gameId       = Nothing
                     , salt         = salt
                     , public       = public
                     , launched     = launched
                     }
  lobby <- liftIO $ insertObject lobbyC
  json $ sucessJson sucessCode lobby
 where
  generateSalt :: IO String
  generateSalt = do
    adjectives <- getWords "./static/wordlist-adjectives.txt"
    nouns      <- getWords "./static/wordlist-nouns.txt"
    let adjective = "cool"
    let noun      = "language"
    adjIdx  <- randomRIO (0, length adjectives - 1)
    nounIdx <- randomRIO (0, length adjectives - 1)

    return ((adjectives !! adjIdx) ++ "-" ++ (nouns !! nounIdx))

  getWords :: FilePath -> IO [String]
  getWords file = do
    contents <- readFile file
    return (lines contents)

{-
@api GET {{base_url}}/lobby/find find
@apiName find
@apiGroup Lobby
@apiDescription find all public lobbys
-}
findLobbys :: (MonadIO m, ListContains n User xs) => ActionCtxT (HVect xs) m b
findLobbys = do
  oldCtx <- getContext
  let host :: User = (findFirst oldCtx)
  let (Key _id)    = uid host

  now <- liftIO getCurrentTime
  let past = addUTCTime (-60 * 60 :: NominalDiffTime) now

  mLobbys <-
    liftIO
      $ (findObjects ["created" =: ["$gte" =: val past], "public" =: val True]
                     [] :: IO [Maybe Lobby]
        )
  let lobbys = [ lobby | (Just lobby) <- mLobbys ]
  json $ sucessJson sucessCode lobbys

{-
@api POST {{base_url}}/lobby/join/:salt join
@apiName join 
@apiGroup Lobby
@apiDescription join a lobby by a salt
-}
joinLobby
  :: (MonadIO m, ListContains n User xs) => String -> ActionCtxT (HVect xs) m b
joinLobby salt = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  elobby <-
    liftIO
    $   findLobby salt
    >>= checkStarted
    >>= checkJoined user
    >>= checkKickedPlayer user
    >>= checkHost user
    >>= userJoinLobby user
  case (elobby) of
    (Left  error) -> json $ errorJson errorJoinLobby error
    (Right lobby) -> do
      setStatus badRequest400
      json $ sucessJson sucessCode lobby
 where
  findLobby :: String -> IO (Either String Lobby)
  findLobby salt = do
    now <- liftIO getCurrentTime
    let past = addUTCTime (-60 * 60 :: NominalDiffTime) now
    mLobby <- liftIO
      (findObject ["created" =: ["$gte" =: val past], "salt" =: val salt] :: IO
          (Maybe Lobby)
      )
    case mLobby of
      Nothing      -> return (Left ("Could not find Lobby with salt: " ++ salt))
      (Just lobby) -> return (Right lobby)

  checkStarted :: Either String Lobby -> IO (Either String Lobby)
  checkStarted (Left  error) = return (Left error)
  checkStarted (Right lobby) = if (launched lobby)
    then return (Left "Game has already started.")
    else return (Right lobby)
  checkJoined :: User -> Either String Lobby -> IO (Either String Lobby)
  checkJoined _ (Left error) = return (Left error)
  checkJoined user (Right lobby) =
    let (Key _id) = uid user
    in  if (_id `elem` (player lobby))
          then return (Left "Already joined Lobby")
          else return (Right lobby)

  checkKickedPlayer :: User -> Either String Lobby -> IO (Either String Lobby)
  checkKickedPlayer _ (Left error) = return (Left error)
  checkKickedPlayer user (Right lobby) =
    let (Key _id) = uid user
    in  if (_id `elem` (kickedPlayer lobby))
          then return (Left "You have been kicked from this lobby")
          else return (Right lobby)

  checkHost :: User -> Either String Lobby -> IO (Either String Lobby)
  checkHost _ (Left error) = return (Left error)
  checkHost user (Right lobby) =
    let (Key _id) = uid user
    in  if (_id == (lobbyHost lobby))
          then return (Left "You cannot join your own Lobby")
          else return (Right lobby)

  userJoinLobby :: User -> (Either String Lobby) -> IO (Either String Lobby)
  userJoinLobby _    (Left  error) = return (Left error)
  userJoinLobby user (Right lobby) = do
    let (Key _id) = uid user
    let newLobby = lobby { player = (_id : (player lobby)) }
    updateObject newLobby
    return (Right newLobby)

{-
@api POST {{base_url}}/lobby/:lobbyId/kick/:userId kick
@apiName kick 
@apiGroup Lobby
@apiDescription kick a player from the Lobby (host is allowed only)
-}
kickPlayer
  :: (MonadIO m, ListContains n User xs)
  => String
  -> String
  -> ActionCtxT (HVect xs) m b
kickPlayer lobbyId playerId = do
  oldCtx <- getContext
  let host :: User = (findFirst oldCtx)
  let (Key hostId) = uid host
  elobby <-
    liftIO
    $   findLobby lobbyId
    >>= isHost hostId
    >>= checkHost playerId
    >>= checkPlayer playerId
    >>= updateLobby playerId
  case elobby of
    (Left  error) -> do 
      setStatus badRequest400
      json $ errorJson errorKickPlayer error
    (Right lobby) -> json $ sucessJson sucessCode lobby
 where
  findLobby :: String -> IO (Either String Lobby)
  findLobby lobbyId = do
    mlobby <- liftIO $ findById lobbyId :: IO (Maybe Lobby)
    case mlobby of
      Nothing      -> return (Left "Lobby not found")
      (Just lobby) -> return (Right lobby)

  isHost :: String -> Either String Lobby -> IO (Either String Lobby)
  isHost _      (Left  error) = return (Left error)
  isHost hostId (Right lobby) = do
    if not $ hostId == (lobbyHost lobby)
      then return (Left "You are not the Host of this lobby")
      else return (Right lobby)

  checkHost :: String -> Either String Lobby -> IO (Either String Lobby)
  checkHost _      (Left  error) = return (Left error)
  checkHost userId (Right lobby) = do
    if userId == (lobbyHost lobby)
      then return (Left "you cannot kick yourself from lobby")
      else return (Right lobby)

  checkPlayer :: String -> Either String Lobby -> IO (Either String Lobby)
  checkPlayer _      (Left  error) = return (Left error)
  checkPlayer userId (Right lobby) = do
    if not (userId `elem` (player lobby))
      then return (Left "player not in lobby")
      else return (Right lobby)

  updateLobby :: String -> (Either String Lobby) -> IO (Either String Lobby)
  updateLobby _      (Left  error) = return (Left error)
  updateLobby userId (Right lobby) = do
    let newLobby = lobby { player = [ x | x <- (player lobby), x /= userId ] }
    updateObject newLobby
    return (Right newLobby)
