{-# LANGUAGE OverloadedStrings #-}

module Integration.AuthTest where
import           Test.Hspec
import           Test.Hspec.Wai
import           Data.ByteString.Lazy.Internal  ( ByteString )

testUserRegister :: ByteString
testUserRegister =
  "{\"email\":\"winderl13@gmail.com\",\"password\":\"supersaveandsecure\",\"username\":\"daehiff1\"}"

errorRegsiter :: ByteString
errorRegsiter =
  "{\"email\"\"winderl13@gmail.com\",\"password\":\"supersaveandsecure\",\"username\":\"daehiff1\"}"

testUserLogin :: ByteString
testUserLogin =
  "{\"email\":\"winderl13@gmail.com\",\"password\":\"supersaveandsecure\"}"

authTest = do
  describe "POST /auth/register" $ do
    it "registers the user correctly" $ do
      post "/auth/register" testUserRegister `shouldRespondWith` 200
      let
        errorResp
          = "{\"error\":{\"code\":3,\"message\":\"user with this email already exists.\"},\"result\":\"failure\"}"
      post "/auth/register" testUserRegister
        `shouldRespondWith` errorResp { matchStatus = 400 }
    it "filters Invalid responses" $ do
      let
        errorResp
          = "{\"error\":{\"code\":3,\"message\":\"Error in $: Failed reading: satisfyWith. Expecting ':' at 'winderl13@gmail.com,password:supersaveandsecure,username:daehiff1}'\"},\"result\":\"failure\"}"
      post "/auth/register" errorRegsiter
        `shouldRespondWith` errorResp { matchStatus = 400 }
  describe "POST /auth/login" $ do
    it "logs the user in" $ do
      post "/auth/login" testUserLogin `shouldRespondWith` 200
    it "checks wrong credentials" $ do
      let
        wrongUserPassword
          = "{\"email\":\"winderl13@gmail.com\",\"password\":\"supersaveandsecuree\"}"
      let
        wrongUserEmail
          = "{\"email\":\"winderl14@gmail.com\",\"password\":\"supersaveandsecure\"}"
      post "/auth/login" wrongUserPassword
        `shouldRespondWith` "{\"error\":{\"code\":2,\"message\":\"invalid password\"},\"result\":\"failure\"}"
                              { matchStatus = 400
                              }
      post "/auth/login" wrongUserEmail
        `shouldRespondWith` "{\"error\":{\"code\":2,\"message\":\"User not avaiable\"},\"result\":\"failure\"}"
                              { matchStatus = 400
                              }
    it "filters Invalid responses" $ do
      let
        errorResp
          = "{\"error\":{\"code\":2,\"message\":\"Error in $: Failed reading: satisfyWith. Expecting ':' at 'winderl13@gmail.com,password:supersaveandsecure,username:daehiff1}'\"},\"result\":\"failure\"}"
      post "/auth/login" errorRegsiter
        `shouldRespondWith` errorResp { matchStatus = 400 }
