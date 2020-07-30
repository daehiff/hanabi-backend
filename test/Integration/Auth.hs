{-# LANGUAGE OverloadedStrings  #-}

module Integration.Auth where
import           Test.Hspec
import           Test.Hspec.Wai
import           Data.ByteString.Lazy.Internal  ( ByteString )
import           Model
import           Model.Utils                    ( ObjectKey(..) )
import           Data.Aeson                     ( decode
                                                , encode
                                                )
import           Data.Aeson.Types        hiding ( String
                                                , Key
                                                )
import           Responses
import           Integration.Utils
import           Utils                          ( flushDB )

beforeEach = do
  flushDB
  return ()


testUser :: User
testUser = User { uid           = NewKey
                , username      = "daehiff"
                , email         = "mail@mailprovider.com"
                , password_hash = Just "supersaveandsecure"
                , sessions      = []
                , pwsalt        = ""
                }

userRegisterJSON :: User -> ByteString
userRegisterJSON user =
  let (Just pw) = password_hash user
  in
    encode
      $ (object
          ["email" .= email user, "password" .= pw, "username" .= username user]
        )

userLoginJSON :: User -> ByteString
userLoginJSON user =
  let (Just pw) = password_hash user
  in  encode $ (object ["email" .= email user, "password" .= pw])


authTest = (before_ beforeEach) $ do
  describe "POST /auth/register" $ do
    it "registers the user correctly" $ do
      post "/auth/register" (userRegisterJSON testUser) `shouldRespondWith` 200
    it "gives correct and expected error messages" $ do
      post "/auth/register" (userRegisterJSON testUser) `shouldRespondWith` 200
      post "/auth/register"
           (userRegisterJSON testUser { username = "mail1@mailprovider.com" })
        `shouldRespondWith` errorResponse
                              400
                              registrationError
                              ("user with this email already exists." :: String)
      post "/auth/register"
           (userRegisterJSON testUser { email = "mail1@mailprovider.com" })
        `shouldRespondWith` errorResponse
                              400
                              registrationError
                              ("Username is already taken." :: String)
  describe "POST /auth/login" $ do
    it "allows registered users to login" $ do
      request <- post "/auth/register" (userRegisterJSON testUser)
      post "/auth/login" (userLoginJSON testUser) `shouldRespondWith` 200
    it "detects invalid usernames and passwords" $ do
      request <- post "/auth/register" (userRegisterJSON testUser)
      post "/auth/login"
           (userLoginJSON testUser { password_hash = Just "wrongpassword" })
        `shouldRespondWith` errorResponse
                              400
                              loginError
                              ("invalid password" :: String)
      post "/auth/login" (userLoginJSON testUser { email = "wrong@mail.com" })
        `shouldRespondWith` errorResponse
                              400
                              loginError
                              ("User not avaiable" :: String)

