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
import           Controller.Game                ( giveHint )

getGameFromResponse :: ByteString -> Either String Game
getGameFromResponse bodyStr = parseBody
  bodyStr
  (\obj -> do
    sucess <- (obj .: "success") :: Parser Object
    lobby  <- (sucess .: "message") :: Parser Game
    return lobby
  )

createGame users gameEnd = do
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
  if gameEnd
    then
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/settings")) [("auth", adminjwt)]
             (encode (Settings {amtLives = 1,
                          amtHints = 8,
                          level = Hard,
                          isRainbow = True}))
    else 
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/settings")) [("auth", adminjwt)]
             (encode Settings {amtLives = 3,
                          amtHints = 8,
                          level = Hard,
                          isRainbow = True})
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
  let adminPlayer =
        filter (\player -> (playerId player) /= _id) (players game1)
  let game = gameA { players = (adminPlayer !! 0 : (players gameA)) }
  return (game, [(admin, adminjwt), (user1, user1jwt)])


gameTest = before_ flushDB $ do
  describe "GET /game/:gameid/status" $ do
    it "returns valid game status" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (Key gameId)      = gid game
      let (Key adminId)     = uid admin
      customGet (packChars ("/game/" ++ gameId ++ "/status"))
                [("auth", adminjwt)]
                ""
        `shouldRespondWith` sucessResponse
                              200
                              sucessCode
                              (game
                                { players = filter
                                              (\player ->
                                                (playerId player) /= adminId
                                              )
                                              (players game)
                                }
                              )
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      (customGet (packChars ("/game/" ++ gameId ++ "/status"))
                 [("auth", user1jwt)]
                 ""
        )
        `shouldRespondWith` sucessResponse
                              200
                              sucessCode
                              (game
                                { players = filter
                                              (\player ->
                                                (playerId player) /= user1Id
                                              )
                                              (players game)
                                }
                              )
    it "someone not in the game tries to get the status" $ do
      (game, userList) <- createGame defaultUsers False
      let user2 = defaultUsers !! 2
      (user2, user2jwt) <- setupUser user2
      let (Key gameId) = gid game
      customGet (packChars ("/game/" ++ gameId ++ "/status"))
                [("auth", user2jwt)]
                ""
        `shouldRespondWith` errorResponse
                              400
                              errorGameStatus
                              ("You are not part of the game!" :: String)

  describe "POST /game/:gameid/move" $ do
    it "make valid hint move" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      customPost
          (packChars ("/game/" ++ gameId ++ "/move"))
          [("auth", adminjwt)]
          (encode HintAction { targetPlayer = user1Id, hint = (Left Red) })
        `shouldRespondWith` 200
    it "make valid play move" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key gameId)      = gid game
      let mcurrPlayer =
            find (\player -> playerId player == adminId) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", adminjwt)]
              (encode PlayAction { cardId = ((cards currPlayer) !! 0) })
            `shouldRespondWith` 200
    it "make valid discard move" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      --need to give a hint first, so not all the hints are available and therefore the discard option is blocked
      customPost
        (packChars ("/game/" ++ gameId ++ "/move"))
        [("auth", adminjwt)]
        (encode HintAction { targetPlayer = user1Id, hint = (Left Red) })
      let mcurrPlayer =
            find (\player -> playerId player == user1Id) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", user1jwt)]
              (encode DiscardAction { cardId = ((cards currPlayer) !! 0) })
            `shouldRespondWith` 200
    it "not able to discard, because the hints are full" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (Key adminId)     = uid admin
      let (Key gameId)      = gid game
      let mcurrPlayer =
            find (\player -> playerId player == adminId) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", adminjwt)]
              (encode DiscardAction { cardId = ((cards currPlayer) !! 0) })
            `shouldRespondWith` errorResponse
                                  400
                                  errorMove
                                  ("Cannot discard, because max amount of hints is reached!" :: String
                                  )
    it "tries to discard someone else's card" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      --need to give a hint first, so not all the hints are available and therefore the discard option is blocked
      customPost
        (packChars ("/game/" ++ gameId ++ "/move"))
        [("auth", adminjwt)]
        (encode HintAction { targetPlayer = user1Id, hint = (Left Red) })
      let mcurrPlayer =
            find (\player -> playerId player == adminId) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", user1jwt)]
              (encode DiscardAction { cardId = ((cards currPlayer) !! 0) })
            `shouldRespondWith` errorResponse
                                  400
                                  errorMove
                                  ("Card not discardable!" :: String)
    it "tries to play someone else's card" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      let mcurrPlayer =
            find (\player -> playerId player == user1Id) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", adminjwt)]
              (encode PlayAction { cardId = ((cards currPlayer) !! 0) })
            `shouldRespondWith` errorResponse
                                  400
                                  errorMove
                                  ("Card not playable!" :: String)
    it "not your turn to give a hint" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (user1, user1jwt) = userList !! 1
      let (Key adminId)     = uid admin
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      let mcurrPlayer =
            find (\player -> playerId player == user1Id) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", user1jwt)]
              (encode HintAction { targetPlayer = adminId, hint = (Left Red) })
            `shouldRespondWith` errorResponse
                                  400
                                  errorMove
                                  ("It's not your turn!" :: String)
    it "tries to hint oneself" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (Key adminId)     = uid admin
      let (Key gameId)      = gid game
      let mcurrPlayer =
            find (\player -> playerId player == adminId) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", adminjwt)]
              (encode HintAction { targetPlayer = adminId, hint = (Left Red) })
            `shouldRespondWith` errorResponse
                                  400
                                  errorMove
                                  ("You cannot hint yourself!" :: String)
    it "not your turn to play" $ do
      (game, userList) <- createGame defaultUsers False
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      let mcurrPlayer =
            find (\player -> playerId player == user1Id) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", user1jwt)]
              (encode PlayAction { cardId = ((cards currPlayer) !! 0) })
            `shouldRespondWith` errorResponse
                                  400
                                  errorMove
                                  ("It's not your turn!" :: String)
    it "not your turn to discard" $ do
      (game, userList) <- createGame defaultUsers False
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      let (Key gameId)      = gid game
      let mcurrPlayer =
            find (\player -> playerId player == user1Id) (players game)
      case mcurrPlayer of
        Nothing -> undefined
        (Just currPlayer) ->
          customPost
              (packChars ("/game/" ++ gameId ++ "/move"))
              [("auth", user1jwt)]
              (encode DiscardAction { cardId = ((cards currPlayer) !! 0) })
            `shouldRespondWith` errorResponse
                                  400
                                  errorMove
                                  ("It's not your turn!" :: String)
    -- it "tries to hint, but game already ended" $ do
    --   (game, userList) <- createGame defaultUsers True
    --   let (admin, adminjwt) = userList !! 0
    --   let (Key adminId)     = uid admin
    --   let (user1, user1jwt) = userList !! 1
    --   let (Key user1Id)     = uid user1
    --   let (Key gameId)      = gid game
    --   let user1Player = [ player | player <- (players game), ((playerId player) == user1Id) ] !! 0
    --   let adminPlayer = [ player | player <- (players game), ((playerId player) == adminId) ] !! 0
    --   -- following random moves to hopefully successfully end the game
    --   customPost
    --       (packChars ("/game/" ++ gameId ++ "/move"))
    --       [("auth", adminjwt)]
    --       (encode PlayAction { cardId = ((cards adminPlayer) !! 0) })
      -- customPost
      --     (packChars ("/game/" ++ gameId ++ "/move"))
      --     [("auth", user1jwt)]
      --     (encode PlayAction { cardId = ((cards user1Player) !! 0) })
      -- customPost
      --     (packChars ("/game/" ++ gameId ++ "/move"))
      --     [("auth", adminjwt)]
      --     (encode PlayAction { cardId = ((cards adminPlayer) !! 0) })
      -- customPost
      --     (packChars ("/game/" ++ gameId ++ "/move"))
      --     [("auth", user1jwt)]
      --     (encode PlayAction { cardId = ((cards user1Player) !! 0) })
      -- customPost
      --     (packChars ("/game/" ++ gameId ++ "/move"))
      --     [("auth", adminjwt)]
      --     (encode PlayAction { cardId = ((cards adminPlayer) !! 0) })
      -- customPost
      --     (packChars ("/game/" ++ gameId ++ "/move"))
      --     [("auth", user1jwt)]
      --     (encode PlayAction { cardId = ((cards user1Player) !! 0) })
      -- method to actually test
      -- customGet (packChars ("/game/" ++ gameId ++ "/move"))
      --           [("auth", adminjwt)]
      --           (encode HintAction { targetPlayer = user1Id, hint = (Left Red) })
      --   `shouldRespondWith` errorResponse
      --                         400
      --                         gameOrPlayerNotFoundError
      --                         ("Game already ended!" :: String)
  describe "GET /game/:gameid/cards" $ do
    it "get all the cards in the game, except of your own" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (Key gameId)      = gid game
      let (Key adminId)     = uid admin
      customGet (packChars ("/game/" ++ gameId ++ "/cards"))
                [("auth", adminjwt)]
                ""
        `shouldRespondWith` 200
      -- test the same for the second player
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      (customGet (packChars ("/game/" ++ gameId ++ "/cards"))
                 [("auth", user1jwt)]
                 ""
        )
        `shouldRespondWith` 200
    it "someone not in the game tries to get the cards" $ do
      (game, userList) <- createGame defaultUsers False
      let user2 = defaultUsers !! 2
      (user2, user2jwt) <- setupUser user2
      let (Key gameId) = gid game
      customGet (packChars ("/game/" ++ gameId ++ "/cards"))
                [("auth", user2jwt)]
                ""
        `shouldRespondWith` errorResponse
                              400
                              gameOrPlayerNotFoundError
                              ("You are not part of the game!" :: String)
  describe "GET /game/:gameid/ownCards" $ do
    it "get own cards without number and color information" $ do
      (game, userList) <- createGame defaultUsers False
      let (admin, adminjwt) = userList !! 0
      let (Key gameId)      = gid game
      let (Key adminId)     = uid admin
      customGet (packChars ("/game/" ++ gameId ++ "/ownCards"))
                [("auth", adminjwt)]
                ""
        `shouldRespondWith` 200
      -- test the same for the second player
      let (user1, user1jwt) = userList !! 1
      let (Key user1Id)     = uid user1
      (customGet (packChars ("/game/" ++ gameId ++ "/ownCards"))
                 [("auth", user1jwt)]
                 ""
        )
        `shouldRespondWith` 200
    it "someone not in the game tries to get own cards" $ do
      (game, userList) <- createGame defaultUsers False
      let user2 = defaultUsers !! 2
      (user2, user2jwt) <- setupUser user2
      let (Key gameId) = gid game
      customGet (packChars ("/game/" ++ gameId ++ "/ownCards"))
                [("auth", user2jwt)]
                ""
        `shouldRespondWith` errorResponse
                              400
                              gameOrPlayerNotFoundError
                              ("You are not part of the game!" :: String)
    -- it "try to get own cards, but game already ended" $ do
    --   (game, userList) <- createGame defaultUsers True
    --   let (admin, adminjwt) = userList !! 0
    --   let (Key adminId)     = uid admin
    --   let (user1, user1jwt) = userList !! 1
    --   let (Key user1Id)     = uid user1
    --   let (Key gameId)      = gid game
    --   let user1Player = [ player | player <- (players game), ((playerId player) == user1Id) ] !! 0
    --   let adminPlayer = [ player | player <- (players game), ((playerId player) == adminId) ] !! 0
    --   -- following random moves to hopefully successfully end the game
    --   customPost
    --       (packChars ("/game/" ++ gameId ++ "/move"))
    --       [("auth", adminjwt)]
    --       (encode PlayAction { cardId = ((cards adminPlayer) !! 0) })
    --   customPost
    --       (packChars ("/game/" ++ gameId ++ "/move"))
    --       [("auth", user1jwt)]
    --       (encode PlayAction { cardId = ((cards user1Player) !! 0) })
    --   customPost
    --       (packChars ("/game/" ++ gameId ++ "/move"))
    --       [("auth", adminjwt)]
    --       (encode PlayAction { cardId = ((cards adminPlayer) !! 0) })
    --   customPost
    --       (packChars ("/game/" ++ gameId ++ "/move"))
    --       [("auth", user1jwt)]
    --       (encode PlayAction { cardId = ((cards user1Player) !! 0) })
    --   customPost
    --       (packChars ("/game/" ++ gameId ++ "/move"))
    --       [("auth", adminjwt)]
    --       (encode PlayAction { cardId = ((cards adminPlayer) !! 0) })
    --   customPost
    --       (packChars ("/game/" ++ gameId ++ "/move"))
    --       [("auth", user1jwt)]
    --       (encode PlayAction { cardId = ((cards user1Player) !! 0) })
    --   -- method to actually test
    --   customGet (packChars ("/game/" ++ gameId ++ "/status"))
    --             [("auth", adminjwt)]
    --             ""
    --     `shouldRespondWith` errorResponse
    --                           400
    --                           gameOrPlayerNotFoundError
    --                           ("Game already ended!" :: String)
    --   customGet (packChars ("/game/" ++ gameId ++ "/ownCards"))
    --             [("auth", adminjwt)]
    --             ""
    --     `shouldRespondWith` errorResponse
    --                           400
    --                           gameOrPlayerNotFoundError
    --                           ("Game already ended!" :: String)
-- TODO: all the game ended tests are still missing