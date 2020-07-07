{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds     #-}

module Init where

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Trans
import           Data.IORef

import qualified Data.Text                     as T

import           Model
import           Model.Utils                     ( findObject
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
import           Handler.Auth                    ( loginHandle
                                                , registerHandle
                                                )

import           Handler.Lobby                   ( createLobby, joinLobby , findLobbys, joinLobby)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)




runApp :: IO ()
runApp = do
  ref      <- newIORef 0
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)


--app::SpockCtxM ctx SqlBackend SessionVal BlogState ()
app = do
  prehook initHook $ do
    get root $ do
      file (T.pack "") "./static/landingPage.html"
    post "/auth/login" $ loginHandle
    post "/auth/register" $ registerHandle
    prehook authHook $ prehook updateJWTHook $ do
      post "/lobby/create" $ createLobby
      get ("/lobby/find") findLobbys
      post ("/lobby/join" <//> var ) $ joinLobby
      
      get ("card" <//> var) $ cardHandler
      get "cards" $ do
        allCards <- liftIO ((findObjects [] []) :: IO ([Maybe Card]))
        json allCards



cardHandler :: MonadIO m => String -> ActionCtxT ctx m b
cardHandler id = do
  card <- liftIO ((findById id) :: IO (Maybe Card))
  json card

