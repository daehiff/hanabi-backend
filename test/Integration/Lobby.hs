{-# LANGUAGE OverloadedStrings #-}

module Integration.Lobby where
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
import           Integration.Utils              ( unwrapJWT )
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
import           Integration.Utils              ( errorResponse
                                                , sucessResponse,
                                                customGet,
                                                customPost
                                                )
import           Responses

import           Controller.Utils               ( parseBody )

beforeAll = do
  flushDB
  return ()


-- Get the default 5 Test users
defaultUsers =
  [ User { uid           = NewKey
         , username      = "dave"
         , email         = "samplemail@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         , pwsalt        = ""
         }
  , User { uid           = NewKey
         , username      = "annika"
         , email         = "samplemail1@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         , pwsalt        = ""
         }
  , User { uid           = NewKey
         , username      = "davidH."
         , email         = "samplemail2@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         , pwsalt        = ""
         }
  , User { uid           = NewKey
         , username      = "patsch"
         , email         = "samplemail3@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         , pwsalt        = ""
         }
  , User { uid           = NewKey
         , username      = "leonBaum"
         , email         = "samplemail4@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         , pwsalt        = ""
         }
  , User { uid           = NewKey
         , username      = "fritzie"
         , email         = "samplemail5@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         , pwsalt        = ""
         }
  ]

setupUser user = do
  request <- request methodPost "auth/register" [] (userRegisterJSON user)
  let (Right respUser) = parseBody
        (simpleBody request)
        (\obj -> do
          sucess <- (obj .: "success") :: Parser Object
          user   <- (sucess .: "message") :: Parser User
          return user
        )
  let jwt = unwrapJWT ((lookup "auth" (simpleHeaders request)))
  return (respUser, jwt)

loginUser user = do
  request <- request methodPost "auth/login" [] (userLoginJSON user)
  let jwt = unwrapJWT ((lookup "auth" (simpleHeaders request)))
  return (user, jwt)

createLobbyJSON :: Bool -> ByteString
createLobbyJSON public =
  encode
    $ (object
        [ "public" .= public
        , "settings" .= Settings { amtLives  = 3
                                 , amtHints  = 8
                                 , level     = Hard
                                 , isRainbow = True
                                 }
        ]
      )


createSettingsJSON =
  encode
    $ (object
        [
          "settings" .= Settings {
                          amtLives = 1,
                          amtHints = 10,
                          level = Easy,
                          isRainbow = False
          }
        ])

getLobbyFromResponse :: ByteString -> Either String Lobby
getLobbyFromResponse bodyStr = parseBody
  bodyStr
  (\obj -> do
    sucess <- (obj .: "success") :: Parser Object
    lobby  <- (sucess .: "message") :: Parser Lobby
    return lobby
  )


lobbyTest = before_ flushDB $ do
  describe "POST /lobby/create" $ do
    it "can create a new public lobby" $ do -- TODO check lobbys/find and private creation
      let admin = defaultUsers !! 0
      (admin, jwt) <- setupUser admin
      (customPost "/lobby/create" [("auth", jwt)] (createLobbyJSON True))
        `shouldRespondWith` 200
    it "can create a new private lobby" $ do
      let admin = defaultUsers !! 0
      (admin, jwt) <- setupUser admin
      (customPost "/lobby/create" [("auth", jwt)] (createLobbyJSON False))
        `shouldRespondWith` 200
    it "returns an error on incorrect input" $ do
      let admin = defaultUsers !! 0
      (admin, jwt) <- setupUser admin
      (customPost "/lobby/create" [("auth", jwt)] "")
        `shouldRespondWith` errorResponse
                              400
                              errorCreateLobby
                              ("Error in $: not enough input" :: String)
  describe "GET /lobby/find" $ do
    it "finds all public lobbys" $ do
      let admin = defaultUsers !! 0
      (admin, jwt) <- setupUser admin
      requestCL    <-
        (customPost "/lobby/create" [("auth", jwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      (customGet "/lobby/find" [("auth", jwt)] "")
        `shouldRespondWith` sucessResponse 200 sucessCode ([] :: [Lobby])
    it "hides private lobbys" $ do
      let admin = defaultUsers !! 0
      (admin, jwt) <- setupUser admin
      requestCL    <-
        (customPost "/lobby/create" [("auth", jwt)] (createLobbyJSON False))
      (customGet "/lobby/find" [("auth", jwt)] "")
        `shouldRespondWith` sucessResponse 200 sucessCode ([] :: [Lobby])
  describe "POST /lobby/join/:salt" $ do
    it "allows a single user to join" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key _id)     = uid user1
      let lobbyJ        = lobby { player = [_id] }
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
        `shouldRespondWith` sucessResponse 200 sucessCode (lobbyJ :: Lobby)
    it "catches invalid input" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", adminjwt)]
                 ""
        `shouldRespondWith` errorResponse
                              400
                              errorJoinLobby
                              ("You cannot join your own Lobby" :: String)

      customPost (packChars ("/lobby/join/stupid-salt")) [("auth", adminjwt)] ""
        `shouldRespondWith` errorResponse
                              400
                              errorJoinLobby
                              ("Could not find Lobby with salt: stupid-salt" :: String)
  describe "POST /lobby/:lobbyId/leave" $ do
    it "allows joined users to leave" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key _id)     = uid user1
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/leave"))
                 [("auth", user1jwt)]
                 ""
        `shouldRespondWith` sucessResponse 200 sucessCode (lobby :: Lobby)
    it "catches invalid input" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key _id)     = uid user1
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
      customPost (packChars ("/lobby/" ++ "invalidID" ++ "/leave"))
                 [("auth", user1jwt)]
                 ""
        `shouldRespondWith` errorResponse 400
                                          errorJoinLobby
                                          ("Lobby not found" :: String)
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/leave"))
                 [("auth", user1jwt)]
                 ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/leave"))
                 [("auth", user1jwt)]
                 ""
        `shouldRespondWith` errorResponse
                              400
                              errorJoinLobby
                              ("You did not join this lobby or you already left." :: String)
  describe "POST /lobby/:lobbyId/kick/:userId" $ do
    it "kicks player and does not allow him to join" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key _id)     = uid user1
      let (Key lobbyId) = lid lobby
      let lobbyK        = lobby { kickedPlayer = [_id] }
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/kick/" ++ _id))
                 [("auth", adminjwt)]
                 ""
        `shouldRespondWith` sucessResponse 200 sucessCode (lobbyK :: Lobby)
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
        `shouldRespondWith` errorResponse
                              400
                              errorJoinLobby
                              ("You have been kicked from this lobby" :: String)
  describe "GET /lobby/:lobbyId/status" $ do
    it "descibes the status of the current Lobby" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      customGet (packChars ("/lobby/" ++ lobbyId ++ "/status"))
                [("auth", adminjwt)]
                ""
        `shouldRespondWith` sucessResponse 200 sucessCode (lobby :: Lobby)

  describe "POST /lobby/:lobbyId/launch" $ do
    it "allows admin to launch the game" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                 [("auth", adminjwt)]
                 ""
        `shouldRespondWith` 200
    it "someone who is not the host of the lobby tries to launch the game" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key _id)     = uid user1
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                 [("auth", user1jwt)]
                 ""
        `shouldRespondWith` errorResponse 400
                                          errorLaunch
                                          ("You are not the Host of this lobby" :: String)
    it "too few players" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                 [("auth", adminjwt)]
                 ""
        `shouldRespondWith` errorResponse 400
                                          errorLaunch
                                          ("To few player in the Lobby." :: String)
    it "too many players" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      let user2 = defaultUsers !! 2
      (user2, user2jwt) <- setupUser user2
      let user3 = defaultUsers !! 3
      (user3, user3jwt) <- setupUser user3
      let user4 = defaultUsers !! 4
      (user4, user4jwt) <- setupUser user4
      let user5 = defaultUsers !! 5
      (user5, user5jwt) <- setupUser user5
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user1jwt)]
                ""
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user2jwt)]
                ""
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user3jwt)]
                ""
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user4jwt)]
                ""
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user5jwt)]
                ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                [("auth", adminjwt)]
                ""
        `shouldRespondWith` errorResponse 400
                                          errorLaunch
                                          ("To many players in the Lobby." :: String)
    it "someone tries to join the lobby, after the game was launched" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      let user2 = defaultUsers !! 2
      (user2, user2jwt) <- setupUser user2
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user1jwt)]
                ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                [("auth", adminjwt)]
                ""
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user2jwt)]
                ""
        `shouldRespondWith` errorResponse 400
                                          errorJoinLobby
                                          ("Game already started!" :: String)
    it "someone tries to leave the lobby, after the game was launched" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user1jwt)]
                ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                [("auth", adminjwt)]
                ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/leave"))
                [("auth", user1jwt)]
                ""
        `shouldRespondWith` errorResponse 400
                                          errorJoinLobby
                                          ("Game already started!" :: String)
    it "admin tries to kick a player, after game was launched" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key _id)     = uid user1
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                [("auth", user1jwt)]
                ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                [("auth", adminjwt)]
                ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/kick/" ++ _id))
                 [("auth", adminjwt)]
                 ""
        `shouldRespondWith` errorResponse 400
                                          errorKickPlayer
                                          ("Game already started!" :: String)
    it "tries to launch game twice" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      customPost (packChars ("/lobby/join/" ++ salt lobby))
                 [("auth", user1jwt)]
                 ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                 [("auth", adminjwt)]
                 ""
      customPost (packChars ("/lobby/" ++ lobbyId ++ "/launch"))
                 [("auth", adminjwt)]
                 ""
        `shouldRespondWith` errorResponse 400 errorLaunch ("Game already started!" :: String)
  describe "POST /lobby/:lobbyId/settings" $ do
    it "successfully change the settings" $ do
      let admin = defaultUsers !! 0
      (admin, adminjwt) <- setupUser admin
      let user1 = defaultUsers !! 1
      (user1, user1jwt) <- setupUser user1
      requestCL         <-
        (customPost "/lobby/create" [("auth", adminjwt)] (createLobbyJSON True))
      let (Right lobby) = getLobbyFromResponse (simpleBody requestCL)
      let (Key lobbyId) = lid lobby
      let newSettings = Settings {amtLives = 1,
                          amtHints = 10,
                          level = Easy,
                          isRainbow = False}
      (customPost (packChars ("/lobby/"++ lobbyId ++ "/settings")) [("auth", adminjwt)] 
                  createSettingsJSON)
          `shouldRespondWith` sucessResponse 200 sucessCode (lobby {gameSettings = newSettings })





