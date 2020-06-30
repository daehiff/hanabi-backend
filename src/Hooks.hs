{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hooks where

import           GHC.Generics
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , encode
                                                , decode
                                                )
import           Data.Bson
import           Data.HVect
import           Web.Spock
import           System.Environment             ( getEnv )
import           Model                          ( User
                                                , findById
                                                , uid
                                                , sessions
                                                )

import           Jose.Jws
import           Jose.Jwa
import           Jose.Jwt                hiding ( decode
                                                , encode
                                                )
import           Control.Monad.Trans
import           Control.Monad                  ( liftM )

import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )
import           Data.Time.Clock.POSIX          ( getPOSIXTime
                                                , POSIXTime
                                                )
import           Data.ByteString.Internal       ( ByteString )
import           Responses                      ( errorJson
                                                , authError
                                                )
{-
SAP Stands for Session Authentication Payload
All Data that is required by the user to send via JWT Token:
  userData
  currentSessionID
  ttl of the JWT
-}
data SAP = SAP { user::User, sessionid::String, ttl::Integer} deriving(Show, Eq, Generic)

instance ToJSON SAP where

instance FromJSON SAP where

{- Helper to get curren timestamp in MS -}
_getNow :: IO Integer
_getNow = (round . (* 1000)) <$> getPOSIXTime

{-
Base Hook returns HNil for initalisation
-}
initHook :: ActionCtxT () (WebStateM () () ()) (HVect '[])
initHook = return HNil

{-
Update the JWT Token after response After (!) auth hook. (TODO can we combine both hooks?)
General Approach:
AUTH valid -> SET new shortlived JWT -> Wait for response
If TTL expired, check session in Database
If session Expried reenforce login

This migitates the database requests per user request for checking the session
-}
updateJWTHook
  :: ListContains n SAP xs
  => ActionCtxT (HVect xs) (WebStateM conn p st) (HVect xs)
updateJWTHook = do
  oldCtx <- getContext
  let oldPayload :: SAP = (findFirst oldCtx)
  now <- liftIO _getNow
  let newTTL  = now + 15 * 60 * 1000 -- new TTL 15 mins
  let payload = oldPayload { ttl = newTTL }
  jwt <- liftIO $ _sessionToJWT payload
  setHeader "auth" jwt
  return oldCtx
 where
  _sessionToJWT :: SAP -> IO T.Text
  _sessionToJWT payload = do
    let (Right jwt) = hmacEncode HS384 "test" (toStrict (encode payload))
    return (T.pack (show (unJwt jwt)))

{-
Auth hook that checks the users JWT
-}
authHook
  :: ActionCtxT (HVect xs) (WebStateM conn p st) (HVect (User ': SAP ': xs))
authHook = do
  oldCtx   <- getContext
  jwt      <- (header "auth")
  mPayload <- liftIO $ _validateSession jwt
  case mPayload of
    (Left  errorMsg) -> json $ errorJson authError errorMsg
    (Right payload ) -> return (user payload :&: payload :&: oldCtx)
 where
  _validateSession :: Maybe T.Text -> IO (Either String SAP)
  _validateSession Nothing    = return (Left "Header: auth is missing")
  _validateSession (Just jwt) = do
    let eitherDecoded = hmacDecode "test" (encodeUtf8 jwt)
    case eitherDecoded of
      (Left error) -> return (Left ("Malformatted JWT: " ++ show error))
      (Right (_, rawPayload)) -> validationProcess rawPayload
   where
    validationProcess :: ByteString -> IO (Either String SAP)
    validationProcess rawPayload = do
      let (Just payload) = (decode (fromStrict rawPayload)) :: Maybe SAP
      now <- _getNow
      let userTtl   = ttl payload
      let tokenUser = user payload
      if (validTTL now userTtl)
        then fetchUser payload
        else return (Right (payload))

    validTTL :: Integer -> Integer -> Bool
    validTTL now ttl = (now - ttl) < 0

    fetchUser :: SAP -> IO (Either String SAP)
    fetchUser payload = do
      let (Just userId) = (uid $ user payload)
      (Just dataBaseUser) <- findById userId :: IO (Maybe User)
      let payload = payload { user = dataBaseUser }
      if (sessionid payload) `elem` (sessions dataBaseUser)
        then return (Right payload)
        else return (Left "Session Expired, please login.")
