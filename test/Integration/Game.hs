{-# LANGUAGE OverloadedStrings #-}

module Integration.Game where
import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai.Test               ( SResponse(..) )
import           Network.HTTP.Types.Header
import           Test.Hspec.Wai.Internal
------------------------------------------------------------
import           Utils                          ( flushDB )
import           Model
import           Model.Utils
import           Web.Spock                      ( spockAsApp )
import           Integration.Auth               ( userRegisterJSON
                                                , userLoginJSON
                                                )
import           Integration.Utils              ( unwrapJWT
                                                , customGet
                                                , customPost
                                                , errorResponse
                                                , sucessResponse
                                                )
import           Network.HTTP.Types.Method      ( methodPost
                                                , methodGet
                                                )
import           Data.ByteString.Lazy.Internal  ( ByteString )
import           Data.ByteString.Internal       ( packChars )

import           Data.Aeson                     ( decode
                                                , encode
                                                )
import           Data.Aeson.Types        hiding ( String
                                                , Key
                                                )
import           Data.List                      ( find )
import           Integration.Lobby              ( defaultUsers
                                                , getLobbyFromResponse
                                                , createLobbyJSON
                                                , setupUser
                                                )
import           Responses

import           Controller.Utils               ( parseBody )
import Controller.Game (giveHint)

-- createHintJSON targetId =
--   encode
--     $ (object
--         [  "target" .= targetId
--         , "color" .= Red
--         , "tag" .= HintAction
--         ]
--       )

-- createPlayJSON cardId =
--   encode
--     $ (object
--         [  "cardId" .= cardId
--         , "tag" .= PlayAction
--         ]
--         )
        
-- createDiscardJSON cardId =
--   encode
--     $ (object
--         [  "cardId" .= cardId
--         , "tag" .= DiscardAction
--         ]
--         )

getGameFromResponse :: ByteString -> Either String Game
getGameFromResponse bodyStr = parseBody
  bodyStr
  (\obj -> do
    sucess <- (obj .: "success") :: Parser Object
    lobby  <- (sucess .: "message") :: Parser Game
    return lobby
  )

createGame users = do
  let admin = users !! 0
  (admin, adminjwt) <- setupUser admin
  let user1 = users !! 1
  (user1, user1jwt) <- setupUser user1
  requestCl         <-
    (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
  let (Right lobby) = getLobbyFromResponse (simpleBody requestCl)
  let (Key _id)     = uid user1
  let (Key lobbyId) = lid lobby
  customPost (packChars ("/lobby/join/" ++ salt lobby)) [("auth", user1jwt)] ""
  requestLl <- customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                          [("auth", adminjwt)]
                          ""
  let (Right launchedLobby) = getLobbyFromResponse (simpleBody requestLl)
  let (Just launchedGameId) = gameId launchedLobby
  requestG <- customGet (packChars ("/game/" ++ launchedGameId ++ "/status"))
                       [("auth", adminjwt)]
                       ""
  let (Right gameA) = getGameFromResponse (simpleBody requestG)
  requestG1 <- customGet (packChars ("/game/" ++ launchedGameId ++ "/status"))
                       [("auth", user1jwt)]
                       ""
  let (Right game1) = getGameFromResponse (simpleBody requestG1)
  let admingPlayer = filter (\player -> (playerId player) /= _id) (players game1)
  let game = gameA {players = (admingPlayer!!0 : (players gameA))}
  return (game, [(admin, adminjwt), (user1, user1jwt)])


gameTest = before_ flushDB $ do
  describe "GET /game/:gameid/status" $ do
    it "returns valid game status" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (Key gameId)      = gid game
      let (Key adminId)     = uid admin
      customGet (packChars ("/game/" ++ gameId ++ "/status"))
                 [("auth", adminjwt)]
                 ""
        `shouldRespondWith` sucessResponse 200 sucessCode (game
          { players = filter (\player -> (playerId player) /= adminId)
                             (players game)
          })
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      (customGet (packChars ("/game/" ++ gameId ++ "/status"))
                 [("auth", user1jwt)]
                 ""
        )
        `shouldRespondWith` sucessResponse 200 sucessCode (game
          { players = filter (\player -> (playerId player) /= user1Id)
                             (players game)})
    it "someone not in the game tries to get the status" $ do
      (game, userList) <- createGame defaultUsers
      let user2 = defaultUsers !! 2
      (user2, user2jwt) <- setupUser user2
      let (Key gameId)      = gid game
      customGet (packChars ("/game/" ++ gameId ++ "/status"))
                 [("auth", user2jwt)]
                 ""
        `shouldRespondWith` errorResponse 400 errorGameStatus ("You are not part of the game!" :: String)

  describe "POST /game/:gameid/move" $ do
    it "make valid hint move" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", adminjwt)]
                  (encode HintAction {targetPlayer = user1Id, hint = (Left Red)})
        `shouldRespondWith` 200
    it "make valid play move" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key gameId)      = gid game
      let mcurrPlayer = find (\player -> playerId player == adminId) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) -> 
          customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", adminjwt)]
                  (encode PlayAction {cardId = ((cards currPlayer) !! 0)})
            `shouldRespondWith` 200
    it "make valid discard move" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      --need to give a hint first, so not all the hints are available and therefore the discard option is blocked
      customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", adminjwt)]
                  (encode HintAction {targetPlayer = user1Id, hint = (Left Red)})
      let mcurrPlayer = find (\player -> playerId player == user1Id) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) -> 
          customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", user1jwt)]
                  (encode DiscardAction {cardId = ((cards currPlayer) !! 0)})
            `shouldRespondWith` 200
    it "not able to discard, because the hints are full" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (Key adminId)     = uid admin
      let (Key gameId)      = gid game
      let mcurrPlayer = find (\player -> playerId player == adminId) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) -> 
          customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", adminjwt)]
                  (encode DiscardAction {cardId = ((cards currPlayer) !! 0)})
            `shouldRespondWith` errorResponse 400 errorMove ("Cannot discard, because max amount of hints is reached!" :: String)
    it "tries to discard someone else's card" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      --need to give a hint first, so not all the hints are available and therefore the discard option is blocked
      customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", adminjwt)]
                  (encode HintAction {targetPlayer = user1Id, hint = (Left Red)})
      let mcurrPlayer = find (\player -> playerId player == adminId) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) -> 
          customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", user1jwt)]
                  (encode DiscardAction {cardId = ((cards currPlayer) !! 0)})
            `shouldRespondWith` errorResponse 400 errorMove ("Card not discardable!"::String)
    it "tries to play someone else's card" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      let mcurrPlayer = find (\player -> playerId player == user1Id) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) -> 
          customPost (packChars ("/game/" ++ gameId ++ "/move")) [("auth", adminjwt)]
                  (encode PlayAction {cardId = ((cards currPlayer) !! 0)})
            `shouldRespondWith` errorResponse 400 errorMove ("Card not playable!"::String)
