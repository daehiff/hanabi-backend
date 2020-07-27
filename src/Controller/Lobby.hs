{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Controller.Lobby where -- no control

import Model
import Model.Utils
import           System.Random                  ( randomRIO )
import Responses
import Web.Spock (ActionCtxT)
generateSalt :: IO String
generateSalt = do
    adjectives <- getWords "./static/wordlist-adjectives.txt"
    nouns      <- getWords "./static/wordlist-nouns.txt"
    let adjective = "cool"
    let noun      = "language"
    adjIdx  <- randomRIO (0, length adjectives - 1)
    nounIdx <- randomRIO (0, length adjectives - 1)
    return ((adjectives !! adjIdx) ++ "-" ++ (nouns !! nounIdx))
  where 
    getWords :: FilePath -> IO [String]
    getWords file = do
      contents <- readFile file
      return (lines contents)


findLobbyById :: String -> ActionCtxT ctx (AppStateM sess) (Either String Lobby)
findLobbyById lobbyId = do
  (mlobby :: Maybe Lobby) <- findById lobbyId
  case mlobby of
    Nothing      -> return (Left "Lobby not found")
    (Just lobby) -> return (Right lobby)
