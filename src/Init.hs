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

import           Handler.Lobby                  ( createLobby
                                                , joinLobby
                                                , findLobbys
                                                , joinLobby
                                                ) 
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                )
import           Data.Pool                      ( withResource )
import           System.Environment             ( getEnv
                                                , lookupEnv
                                                )
import Responses
-- TODO (1) create config available App wide
--      (2) refactor insert/.../
--      (3) remove unit tests


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
  return AppConfig { dbConf = dbConf, port = 8080, jwtSecret="psssst!" }

runApp :: IO ()
runApp = do
  ref      <- newIORef 0
  config   <- createConfig
  pool     <- setupDB $ dbConf config
  spockCfg <- defaultSpockCfg () (PCPool pool) config
  runSpock (port config) (spock spockCfg app)


app :: App ()
app = do
{-   middleware (staticPolicy (addBase "static")) $ do
      get "/doc" $ do
        file (T.pack "") "./static/doc/index.html"   -}
  prehook initHook $ do
    middleware (staticPolicy (addBase "./static/doc"))
    get "/doc" $ do
      file (T.pack "") "./static/doc/index.html"
    get root $ do
      file (T.pack "") "./static/landingPage.html"
    post "/auth/login" $ loginHandle
    post "/auth/register" $ registerHandle
    prehook authHook $ prehook updateJWTHook $ do
      post "/lobby/create" $ createLobby
      get ("/lobby/find") findLobbys
      post ("/lobby/join" <//> var) $ joinLobby 



