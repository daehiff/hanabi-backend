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
                                                , setupDB
                                                , DBConf(..)
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


type App ctx = SpockCtxM ctx (Bool, Pipe) () () ()



runApp :: IO ()
runApp = do
  ref      <- newIORef 0
  pool     <- setupDB DBConf{hostUrl="", dbUser="", dbPass="", useReplica=True}
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runSpock 8080 (spock spockCfg app)


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


-- $> :t app 



-- $> :t middleware
cardHandler :: MonadIO m => String -> ActionCtxT ctx m b
cardHandler id = do
  card <- liftIO ((findById id) :: IO (Maybe Card))
  json card

