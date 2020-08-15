{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Lobby where

import           Web.Spock
import           Web.Spock.Config
import           Responses
import           Network.HTTP.Types             ( badRequest400 )
import           Data.Text               hiding ( unfoldr
                                                , take
                                                , lines
                                                , length
                                                , map
                                                )
import qualified Data.Text                     as T
import           Data.List                      ( unfoldr )
import           Data.Bson                      ( val
                                                , (=:)
                                                )
import           Data.HVect              hiding ( length
                                                , (!!)
                                                )
import           Data.Time.Clock
import           Model
import           Model.Utils
import           Control.Monad.Trans
import           Controller.Lobby               ( findLobbyById
                                                , generateSalt
                                                )
import           Controller.Utils               ( parseBody )
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                , ByteString
                                                )

import           Data.Aeson                     ( encode )
import           Data.Aeson.Types               ( Parser
                                                , (.:)
                                                )
import           Database.MongoDB               ( Pipe )
import           Control.Monad.Trans.Reader     ( ReaderT )

import           Controller.Utils               ( getCurretISOTime )
import           Controller.Game                ( createGame )
import           Control.Monad                  ( forM )


findAvailableLobbys :: Maybe String -> Bool -> AppHandle (HVect xs) ([Lobby])
findAvailableLobbys Nothing public = do -- TODO is here a error?
  now <- liftIO getCurretISOTime
  let past = addUTCTime (-60 * 60 :: NominalDiffTime) (now)
  (mLobbys :: [Maybe Lobby]) <- findObjects
    ["created" =: ["$gte" =: val past], "public" =: val public]
    []
  let lobbys = [ lobby | (Just lobby) <- mLobbys ]
  return lobbys
findAvailableLobbys (Just salt) public = do
  now <- liftIO getCurretISOTime
  let past = addUTCTime (-60 * 60 :: NominalDiffTime) (now)
  (mLobbys :: [Maybe Lobby]) <- findObjects
    [ "created" =: ["$gte" =: val past]
    , "public" =: val public
    , "salt" =: val salt
    ]
    []
  let lobbys = [ lobby | (Just lobby) <- mLobbys ]
  return lobbys

{-
  @api {post} {{base_url}}/lobby/create create Lobby
  @apiName create 
  @apiGroup Lobby
  @apiHeader {String} auth Users auth Token.
  @apiDescription create a new Lobby
  @apiErrorExample {json} Sample Input
{
  "public": false
}
-}
createLobby :: (ListContains n User xs) => AppHandle (HVect xs) ()
createLobby = do
  oldCtx     <- getContext
  rawBodyStr <- fromStrict <$> body
  let epublic = parseBody
        rawBodyStr
        (\obj -> do
          isPublic <- (obj .: "public") :: Parser Bool
          return isPublic
        )
  case epublic of
    (Left error) -> do
      setStatus badRequest400
      json $ errorJson errorCreateLobby error
    (Right public) -> do
      lobbys <- findAvailableLobbys Nothing True
      let salts        = [ salt lobby | lobby <- lobbys ]

      let host :: User = (findFirst oldCtx)
      let (Key _id)    = uid host
      now  <- liftIO getCurretISOTime
      salt <- liftIO $ generateSalt salts
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
      lobby <- insertObject lobbyC
      json $ sucessJson sucessCode lobby

{-
@api {get} {{base_url}}/lobby/find find Lobbys 
@apiName find
@apiGroup Lobby
@apiDescription find all public lobbys of the last hour
@apiHeader {String} auth Users auth Token.
-}
findLobbys :: (ListContains n User xs) => AppHandle (HVect xs) ()
findLobbys = do
  oldCtx <- getContext
  let host :: User = (findFirst oldCtx)
  let (Key _id)    = uid host

  now <- liftIO getCurretISOTime
  let past = addUTCTime (-60 * 60 :: NominalDiffTime) (now)
  lobbys <- findAvailableLobbys Nothing True
  json $ (sucessJson sucessCode lobbys)

{-
@api {post} {{base_url}}/lobby/join/:salt join Lobby
@apiName join 
@apiGroup Lobby
@apiParam {String} salt Unique salt of the lobby
@apiHeader {String} auth Users auth Token.
@apiDescription join a lobby by a given salt

-}
joinLobby :: (ListContains n User xs) => String -> AppHandle (HVect xs) () -- TODO propoer Type Annotations
joinLobby salt = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  elobby <-
    findLobbyBySalt salt
    >>= checkStarted
    >>= checkJoined user
    >>= checkKickedPlayer user
    >>= checkHost user
    >>= userJoinLobby user
  case (elobby) of
    (Left error) -> do
      setStatus badRequest400
      json $ errorJson errorJoinLobby error
    (Right lobby) -> do
      json $ sucessJson sucessCode lobby
 where
  findLobbyBySalt :: String -> AppHandle (HVect xs) (Either String Lobby)
  findLobbyBySalt salt = do
    now <- liftIO getCurrentTime
    let past = addUTCTime (-60 * 60 :: NominalDiffTime) now
    (mLobby :: Maybe Lobby) <- findObject
      ["created" =: ["$gte" =: val past], "salt" =: val salt]
    case mLobby of
      Nothing      -> return (Left ("Could not find Lobby with salt: " ++ salt))
      (Just lobby) -> return (Right lobby)

  checkStarted
    :: Either String Lobby -> AppHandle (HVect xs) (Either String Lobby)
  checkStarted (Left  error) = return (Left error)
  checkStarted (Right lobby) = if (launched lobby)
    then return (Left "Game has already started.")
    else return (Right lobby)

  checkJoined
    :: User -> Either String Lobby -> AppHandle (HVect xs) (Either String Lobby)
  checkJoined _ (Left error) = return (Left error)
  checkJoined user (Right lobby) =
    let (Key _id) = uid user
    in  if (_id `elem` (player lobby))
          then return (Left "Already joined Lobby")
          else return (Right lobby)
  checkKickedPlayer
    :: User -> Either String Lobby -> AppHandle (HVect xs) (Either String Lobby)
  checkKickedPlayer _ (Left error) = return (Left error)
  checkKickedPlayer user (Right lobby) =
    let (Key _id) = uid user
    in  if (_id `elem` (kickedPlayer lobby))
          then return (Left "You have been kicked from this lobby")
          else return (Right lobby)

  checkHost
    :: User -> Either String Lobby -> AppHandle (HVect xs) (Either String Lobby)
  checkHost _ (Left error) = return (Left error)
  checkHost user (Right lobby) =
    let (Key _id) = uid user
    in  if (_id == (lobbyHost lobby))
          then return (Left "You cannot join your own Lobby")
          else return (Right lobby)

  userJoinLobby
    :: User -> Either String Lobby -> AppHandle (HVect xs) (Either String Lobby)
  userJoinLobby _    (Left  error) = return (Left error)
  userJoinLobby user (Right lobby) = do
    let (Key _id) = uid user
    let newLobby = lobby { player = (_id : (player lobby)) }
    updateObject newLobby
    return (Right newLobby)

{-
@api {post} {{base_url}}/lobby/:lobbyId/leave leave Lobby
@apiName leave 
@apiGroup Lobby
@apiParam {String} lobbyId UUID of the Lobby
@apiHeader {String} auth Users auth Token
@apiDescription leave a lobby you have joined previously 
-}
leaveLobby :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
leaveLobby lobbyId = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  elobby <-
    findLobbyById lobbyId
    >>= isHost user
    >>= checkPlayerIsInLobby user
    >>= updateLobby user
  case elobby of
    (Left error) -> do
      setStatus badRequest400
      json $ errorJson errorJoinLobby error
    (Right lobby) -> json $ sucessJson sucessCode lobby
 where
  isHost _ (Left error) = return (Left error)
  isHost user (Right lobby) =
    let (Key _id) = uid user
    in  if _id == lobbyHost lobby
          then (return (Left "You as a host cannot leave your lobby."))
          else (return (Right lobby))

  checkPlayerIsInLobby _ (Left error) = return (Left error)
  checkPlayerIsInLobby user (Right lobby) =
    let (Key _id) = uid user
    in  if not $ _id `elem` player lobby
          then return (Left "You did not join this lobby or you already left.")
          else return (Right lobby)

  updateLobby _    (Left  error) = return (Left error)
  updateLobby user (Right lobby) = do
    let (Key _id) = uid user
    let newLobby = lobby { player = [ x | x <- (player lobby), x /= _id ] }
    updateObject newLobby
    return (Right newLobby)

{-
@api {post} {{base_url}}/lobby/:lobbyId/kick/:userId kick from Lobby
@apiName kick 
@apiGroup Lobby
@apiParam {String} lobbyId UUID of the lobby you are host
@apiParam {String} userId UUID of the user you want to kick 
@apiHeader {String} auth Users auth Token
@apiDescription kick a player from the Lobby (host is allowed only)
-}
kickPlayer
  :: (ListContains n User xs) => String -> String -> AppHandle (HVect xs) ()
kickPlayer lobbyId playerId = do
  oldCtx <- getContext
  let host :: User = (findFirst oldCtx)
  let (Key hostId) = uid host
  elobby <-
    findLobbyById lobbyId
    >>= isHost hostId
    >>= checkHost playerId
    >>= checkPlayer playerId
    >>= updateLobby playerId
  case elobby of
    (Left error) -> do
      setStatus badRequest400
      json $ errorJson errorKickPlayer error
    (Right lobby) -> json $ sucessJson sucessCode lobby
 where
  --isHost :: String -> Either String Lobby -> IO (Either String Lobby)
  isHost _      (Left  error) = return (Left error)
  isHost hostId (Right lobby) = do
    if not $ hostId == (lobbyHost lobby)
      then return (Left "You are not the Host of this lobby")
      else return (Right lobby)

  --checkHost :: String -> Either String Lobby -> IO (Either String Lobby)
  checkHost _      (Left  error) = return (Left error)
  checkHost userId (Right lobby) = do
    if userId == (lobbyHost lobby)
      then return (Left "you cannot kick yourself from lobby")
      else return (Right lobby)

  --checkPlayer :: String -> Either String Lobby -> IO (Either String Lobby)
  checkPlayer _      (Left  error) = return (Left error)
  checkPlayer userId (Right lobby) = do
    if not (userId `elem` (player lobby))
      then return (Left "player not in lobby")
      else return (Right lobby)

  --updateLobby :: String -> (Either String Lobby) -> IO (Either String Lobby)
  updateLobby _      (Left  error) = return (Left error)
  updateLobby userId (Right lobby) = do
    let newLobby = lobby { player = [ x | x <- (player lobby), x /= userId ]
                         , kickedPlayer = userId : (kickedPlayer lobby)
                         }

    updateObject newLobby
    return (Right newLobby)


{-
@api {get} {{base_url}}/lobby/:lobbyId/status Lobby status
@apiName status 
@apiGroup Lobby
@apiParam {String} lobbyId Id of the current Lobby
@apiHeader {String} auth Users auth Token.
@apiDescription get the status of the current lobby (in case you are a part of it)
-}
getStatus :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
getStatus lobbyId = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  let (Key userId) = uid user
  eLobby <- findLobbyById lobbyId >>= isInLobby userId
  case eLobby of
    (Left  error) -> json $ errorJson errorJoinLobby error
    (Right lobby) -> json $ sucessJson sucessCode lobby
 where
  isInLobby _      (Left  error) = return (Left error)
  isInLobby userId (Right lobby) = do
    if not $ userId `elem` ((lobbyHost lobby) : (player lobby))
      then return (Left "You are not a memeber of this lobby")
      else return (Right lobby)

{-
@api {post} {{base_url}}/lobby/:lobbyId/launch launch Game
@apiName launch 
@apiGroup Lobby
@apiParam {String} lobbyId Id of the current Lobby 
@apiHeader {String} auth Users auth Token.
@apiDescription launch a game (players must be 2<=p<=5)
-}
launchGame :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
launchGame lobbyId = do
  elobby <- findLobbyById lobbyId >>= areEnoughPlayer >>= updateLobby
  case elobby of
    (Left error) -> do
      setStatus badRequest400
      json $ errorJson errorLaunch error
    (Right lobby) -> do
      json $ sucessJson sucessCode lobby
 where
  --areEnoughPlayer :: Either String Lobby -> IO (Either String Lobby)
  areEnoughPlayer (Left  error) = return (Left error)
  areEnoughPlayer (Right lobby) = do
    if (length ((lobbyHost lobby) : player lobby)) < 2
      then return (Left "To few player in the Lobby.")
      else do
        if ((length (player lobby)) > 5)
          then return (Left "To much player are in the Lobby.")
          else return (Right lobby)
  updateLobby
    :: Either String Lobby -> AppHandle (HVect xs) (Either String Lobby)
  updateLobby (Left  error) = return (Left error)
  updateLobby (Right lobby) = do
    let settings = Settings { amtLives  = 3
                            , amtHints  = 8
                            , level     = Hard
                            , isRainbow = True
                            }
    (userObjects :: [Maybe User]) <- forM ((lobbyHost lobby) : (player lobby)) findById
    game <- createGame [user | (Just user) <- userObjects] settings
    let (Key _gid) = gid game
    let newLobby = lobby { launched = True, gameId = Just _gid }
    updateObject newLobby
    return (Right newLobby)


