{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson                     ( encode
                                                , decode
                                                )
import           Database.MongoDB

import           Control.Monad.Trans
import           Data.IORef

import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Text                     as T

import           Users                          ( userRoute )
import           Model



data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)


cardToJson (Just card) = encode card
cardToJson Nothing     = ""

main :: IO ()
main = runApp

runApp :: IO ()
runApp = do
  ref      <- newIORef 0
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app = do
  get root $ do
    file (T.pack "") "./static/landingPage.html"
  get "cards" $ do
    allCards <- liftIO ((findObjects [] []) :: IO ([Maybe Card]))
    json allCards
  get ("card" <//> var) $ cardHandler
  userRoute


cardHandler :: MonadIO m => String -> ActionCtxT ctx m b
cardHandler id = do
  card <- liftIO ((findById id) :: IO (Maybe Card))
  json card


-- $> findObjects [] [] :: IO [Maybe Card]
