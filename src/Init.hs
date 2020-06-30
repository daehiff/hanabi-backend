{-# LANGUAGE TemplateHaskell #-}
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

import           Data.HVect
import           Database.MongoDB
import           System.Environment             ( getEnv )
import           Hooks                          ( initHook, authHook, updateJWTHook )


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
    prehook authHook $ prehook updateJWTHook $ do
      get ("card" <//> var) $ cardHandler
      get "cards" $ do
      allCards <- liftIO ((findObjects [] []) :: IO ([Maybe Card]))
      json allCards
    


cardHandler :: MonadIO m => String -> ActionCtxT ctx m b
cardHandler id = do
  card <- liftIO ((findById id) :: IO (Maybe Card))
  json card

