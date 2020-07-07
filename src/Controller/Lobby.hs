module Controller.Lobby where -- no control

import Model
import Model.Utils
import           System.Random                  ( randomRIO )

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


findLobbyById :: String -> IO (Either String Lobby)
findLobbyById lobbyId = do
  mlobby <- findById lobbyId :: IO (Maybe Lobby)
  case mlobby of
    Nothing      -> return (Left "Lobby not found")
    (Just lobby) -> return (Right lobby)


checkJoined :: String -> User -> Either String Lobby -> IO (Either String Lobby)
checkJoined _ _ (Left error) = return (Left error)
checkJoined errorMsg user (Right lobby) =
  let (Key _id) = uid user
  in  if (_id `elem` (player lobby))
        then return (Left errorMsg)
        else return (Right lobby)