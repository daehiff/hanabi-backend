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
import           Web.Spock                      ( spockAsApp
                                                )

beforeAll = do
  flushDB
  setupUsers
  return ()

{- afterAll = do
  --flushDB
  return () -}

-- Get the default 5 Test users
defaultUsers =
  [ User { uid           = NewKey
         , username      = "dave"
         , email         = "samplemail@mail.com"
         , password_hash = Nothing
         , sessions      = []
         }
  , User { uid           = NewKey
         , username      = "annika"
         , email         = "samplemail1@mail.com"
         , password_hash = Nothing
         , sessions      = []
         }
  , User { uid           = NewKey
         , username      = "davidH."
         , email         = "samplemail2@mail.com"
         , password_hash = Nothing
         , sessions      = []
         }
  , User { uid           = NewKey
         , username      = "patsch"
         , email         = "samplemail3@mail.com"
         , password_hash = Nothing
         , sessions      = []
         }
  , User { uid           = NewKey
         , username      = "leonBaum"
         , email         = "samplemail4@mail.com"
         , password_hash = Nothing
         , sessions      = []
         }
  ]

-- Setup 5 test user
setupUsers = do
  usersIns <- sequence [ user | user <- (map insertObject defaultUsers) ]
  return usersIns

getCurrentUsers = do
  usersFound_ <- findObjects [] [] :: IO [Maybe User]
  return [ user | (Just user) <- usersFound_ ]


lobbyTest = do
  describe "whatever" $ do
    it "does what i want" $ do
      --app <- (spockAsApp testApp) 
      --response <- (runWaiSession (post "/auth/login" "") app)
      --putStrLn $ show $ response
      (post "/auth/login" "") `shouldRespondWith` 400


