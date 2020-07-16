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
  let jwt = unwrapJWT ((lookup "auth" (simpleHeaders request)))
  return (user, jwt)

loginUser user = do
  request <- request methodPost "auth/login" [] (userLoginJSON user)
  let jwt = unwrapJWT ((lookup "auth" (simpleHeaders request)))
  return (user, jwt)

createLobbyJSON :: Bool -> ByteString
createLobbyJSON public = encode $ (object ["public" .= public])


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
      requestCL    <- (customPost "/lobby/create" [("auth", jwt)] (createLobbyJSON True))
      let (Right lobby) =
            (parseBody
              (simpleBody requestCL)
              (\obj -> do
                sucess <- (obj .: "sucess") :: Parser Object
                lobby <- (sucess .: "message") :: Parser Lobby
                return lobby))
      liftIO $ putStrLn (show lobby)
      (customGet "/lobby/find" [("auth", jwt)] "")
        `shouldRespondWith` sucessResponse 200 sucessCode ([lobby] :: [Lobby])
    it "hides private lobbys" $ do 
      let admin = defaultUsers !! 0
      (admin, jwt) <- setupUser admin 
      requestCL    <- (customPost "/lobby/create" [("auth", jwt)] (createLobbyJSON False))
      (customGet "/lobby/find" [("auth", jwt)] "")
        `shouldRespondWith` sucessResponse 200 sucessCode ([] :: [Lobby])

customPost = request methodPost
customGet = request methodGet


