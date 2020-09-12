{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Controller.Lobby where

import           Model
import           Model.Utils
import           System.Random                  ( randomRIO )
import           Responses
import           Web.Spock                      ( ActionCtxT )


generateSalt :: [String] -> IO String
generateSalt salts = do
  adjectives <- getWords "./static/wordlist-adjectives.txt"
  nouns      <- getWords "./static/wordlist-nouns.txt"
  adjIdx     <- randomRIO (0, length adjectives - 1)
  nounIdx    <- randomRIO (0, length nouns - 1)
  let salt = ((adjectives !! adjIdx) ++ "-" ++ (nouns !! nounIdx))
  if salt `elem` salts then generateSalt salts else return salt
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
