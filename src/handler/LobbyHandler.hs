{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LobbyHandler where
import           Web.Spock
import qualified Data.Text                     as T

import BSONExtention (ObjectKey(..))
import           Data.Time.Clock
import           Model(User(..))
import           Web.Spock.Config
import           Control.Monad.Trans
import           Data.HVect

createLobby :: (MonadIO m, ListContains n User xs) => ActionCtxT (HVect xs) m b
createLobby = do
  oldCtx <- getContext
  let host :: User = (findFirst oldCtx)
  let (Key _id) = uid host
  --host <- undefined -- TODO get Player from Context
  --now  <- getCurrentTime
  --let salt     = generateSalt -- TODO one DB action here
  --let public   = False -- TODO parse this from body
  --let launched = False
  json $ T.pack ( _id)
