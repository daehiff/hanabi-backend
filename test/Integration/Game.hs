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
import           Integration.Lobby              ( defaultUsers
                                                , getLobbyFromResponse
                                                , createLobbyJSON
                                                , setupUser
                                                )
import           Responses

import           Controller.Utils               ( parseBody )


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
  let (Right game) = getGameFromResponse (simpleBody requestG)
  return (game, [(admin, adminjwt), (user1, user1jwt)])




gameTest = before_ flushDB $ do
  describe "GET /game/:gameid/status" $ do
    it "returns valid game status" $ do
      (game, userList) <- createGame defaultUsers
      let (admin, adminjwt) = userList !! 0
      let (Key gameId)      = gid game
      (customGet (packChars ("/game/" ++ gameId ++ "/status"))
                 [("auth", adminjwt)]
                 ""
        )
        `shouldRespondWith` sucessResponse 200 sucessCode (game)
  -- TODO test if user which is not in game can get the status
