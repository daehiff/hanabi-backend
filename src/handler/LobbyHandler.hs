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


createLobby :: (MonadIO m, ListContains n User xs) => ActionCtxT (HVect xs) m b
createLobby = do
  oldCtx <- getContext
  let host :: User = (findFirst oldCtx)
  let (Key _id)    = uid host
  now  <- liftIO getCurrentTime
  salt <- liftIO generateSalt
  let public   = False
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
  json lobby
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


