{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds     #-}

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
                                                )

import           Data.HVect
import           Database.MongoDB
import           System.Environment             ( getEnv )
import           Hooks                          ( initHook
                                                , authHook
                                                , updateJWTHook
                                                )
import           Handler.Auth                   ( loginHandle
                                                , registerHandle
                                                )

import           Handler.Lobby


type App ctx = Web.Spock.Core.SpockCtxT ctx (WebStateM () () ()) ()

runApp :: IO ()
runApp = do
  ref      <- newIORef 0
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)


app :: App ()
app = do
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


cardHandler :: MonadIO m => String -> ActionCtxT ctx m b
cardHandler id = do
  card <- liftIO ((findById id) :: IO (Maybe Card))
  json card

