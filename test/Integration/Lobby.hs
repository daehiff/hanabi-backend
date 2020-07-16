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
import           Utils                          ( testApp )
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
                                                , sucessResponse
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
         }
  , User { uid           = NewKey
         , username      = "annika"
         , email         = "samplemail1@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         }
  , User { uid           = NewKey
         , username      = "davidH."
         , email         = "samplemail2@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         }
  , User { uid           = NewKey
         , username      = "patsch"
         , email         = "samplemail3@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         }
  , User { uid           = NewKey
         , username      = "leonBaum"
         , email         = "samplemail4@mail.com"
         , password_hash = Just "supersaveandsecure"
         , sessions      = []
         }
  ]

setupUser user = do
  request <- request methodPost "auth/register" [] (userRegisterJSON user)
  let (Right respUser) = parseBody
        (simpleBody request)
        (\obj -> do
          sucess <- (obj .: "sucess") :: Parser Object
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
createLobbyJSON public = encode $ (object ["public" .= public])

getLobbyFromResponse :: ByteString -> Either String Lobby
getLobbyFromResponse bodyStr = parseBody
  bodyStr
  (\obj -> do
    sucess <- (obj .: "sucess") :: Parser Object
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
        `shouldRespondWith` sucessResponse 200 sucessCode ([lobby] :: [Lobby])
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
        `shouldRespondWith` errorResponse 400 errorJoinLobby ("You cannot join your own Lobby" :: String) 

      customPost (packChars ("/lobby/join/stupid-salt")) [("auth", adminjwt)] ""
        `shouldRespondWith` errorResponse
                              400
                              errorJoinLobby
                              ("Could not find Lobby with salt: stupid-salt" :: String)
  --describe "GET /lobby:lobbyId/status"

customPost = request methodPost
customGet = request methodGet


