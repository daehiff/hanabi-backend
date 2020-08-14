{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
module Init where

import           Web.Spock
import qualified Web.Spock.Core
import           Web.Spock.Config
import           Network.Wai.Middleware.Static
import           Control.Monad.Trans
import           Data.IORef

import qualified Data.Text                     as T

import           Model
import           Model.Utils                    ( findObject
                                                , insertObject
                                                , updateObject
                                                , findObjects
                                                , findById
                                                , setupDB
                                                , DBConf(..)
                                                , MongoObject(..)
                                                )

import           Data.HVect
import           Database.MongoDB
import           Data.Pool                      ( Pool )
import           System.Environment             ( getEnv )
import           Hooks                          ( initHook
                                                , authHook
                                                , updateJWTHook
                                                )
import           Handler.Auth                   ( loginHandle
                                                , registerHandle
                                                )

import           Handler.Lobby                  
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                )
import           Data.Pool                      ( withResource )
import           System.Environment             ( getEnv
                                                , lookupEnv
                                                )
import Responses

-----
import Model.BSONExtention (ObjectKey(..))

createConfig :: IO AppConfig
createConfig = do
  hostUrl     <- getEnv "DB_ADDR"
  dbName      <- getEnv "DB_NAME"
  useReplicaS <- (getEnv "DB_USE_REPLICA")
  let useReplica = (useReplicaS == "true")
  dbUser <- (getEnv "DB_USER")
  dbPw   <- (getEnv "DB_PW")
  let dbConf = DBConf { hostUrl    = hostUrl
                      , dbUser     = dbUser
                      , dbPass     = dbPw
                      , useReplica = useReplica
                      , dbName     = dbName
                      }
  port <- read <$> getEnv "PORT"
  return AppConfig { dbConf = dbConf, port = port, jwtSecret="test" }


runApp :: IO ()
runApp = do
  ref      <- newIORef 0
  config   <- createConfig
  pool     <- setupDB $ dbConf config
  spockCfg <- defaultSpockCfg () (PCPool pool) config
  runSpock (port config) (spock spockCfg app)


app :: App ()
app = do
  get ("/game/test") $ test
  prehook initHook $ do
    middleware (staticPolicy (addBase "./static/doc"))
    get root $ do
      file (T.pack "") "./static/landingPage.html"
    get "/doc" $ do
      file (T.pack "") "./static/doc/index.html"
    post "/auth/login" $ loginHandle
    post "/auth/register" $ registerHandle
    prehook authHook $ prehook updateJWTHook $ do
      -- Lobby Routes
      post "/lobby/create" $ createLobby
      get ("/lobby/find") findLobbys
      post ("/lobby/join" <//> var) $ joinLobby
      post ("/lobby/" <//> var <//> "leave") $ leaveLobby
      post ("/lobby" <//> var <//> "kick" <//> var) $ kickPlayer
      get ("/lobby" <//> var <//> "status") $ getStatus
      post ("/lobby" <//> var <//> "launch") $ launchGame

test :: AppHandle () ()
test = do
  let game = Game {gid = NewKey, currentPlayer = "1234", players = ["12", "123", "1234"], hints = 8, lives = 2, drawPile = ["23", "345"],discardPile = ["453", "4453"], redPile = 3, greenPile = 2, bluePile = 0, yellowPile = 5, whitePile = 2, rainbowPile = 4}
  game_ <- insertObject game
  json game_


