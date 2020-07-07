{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

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
                                                , uid
                                                , sessions
                                                )
import           Model.Utils

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
data SAP = SAP { user::User, sessionid::String, ttl::Integer}
                 deriving(Show, Eq, Generic, ToJSON, FromJSON)


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
  jwt <- liftIO $ sessionToJWT payload
  setHeader "auth" jwt
  return oldCtx


sessionToJWT :: SAP -> IO T.Text
sessionToJWT payload = do
  let (Right jwt) = hmacEncode HS384 "test" (toStrict (encode payload))
  return (T.pack (show (unJwt jwt)))

{-
Auth hook that checks the users JWT
-}
authHook
  :: ActionCtxT (HVect xs) (WebStateM conn p st) (HVect (User ': SAP ': xs))
authHook = do
  oldCtx             <- getContext
  jwt                <- (header "auth")
  rawPayload         <- liftIO $ getRawPayload jwt
  unValidatedPayload <- liftIO $ convertPayloadToSAP rawPayload
  ePayload           <- liftIO $ validatePayload unValidatedPayload
  case ePayload of
    (Left  errorMsg) -> json $ errorJson authError errorMsg
    (Right payload ) -> return (user payload :&: payload :&: oldCtx)
 where
  getRawPayload :: Maybe T.Text -> IO (Either String ByteString)
  getRawPayload Nothing    = return (Left "Header: `auth` is missing")
  getRawPayload (Just jwt) = do
    let eitherDecoded = hmacDecode "test" (encodeUtf8 jwt)
    case eitherDecoded of
      (Left error) -> return (Left ("Unable to parse JWT: " ++ show error))
      (Right (_, payload)) -> return (Right payload)

  convertPayloadToSAP :: Either String ByteString -> IO (Either String SAP)
  convertPayloadToSAP (Left  error     ) = return (Left error)
  convertPayloadToSAP (Right rawPayload) = do
    let payload = (decode (fromStrict rawPayload)) :: Maybe SAP
    case payload of
      Nothing -> return (Left "Error Parsing Payload, try to login again")
      (Just payload) -> return (Right payload)

  validatePayload :: Either String SAP -> IO (Either String SAP)
  validatePayload (Left  error  ) = return (Left error)
  validatePayload (Right payload) = do
    now <- _getNow
    let userTtl   = ttl payload
    let tokenUser = user payload
    if (isValidTTL now userTtl)
      then return (Right payload)
      else
        (\_ -> do
            let (Key userId) = (uid $ user payload)
            mUser <- findById userId :: IO (Maybe User)
            if mUser == Nothing
              then return (Left "Could not verify user, please login again.")
              else
                (\_ -> do
                    let (Just user) = mUser
                    let newPayload  = payload { user = user }
                    if (sessionid newPayload) `elem` (sessions user)
                      then return (Right newPayload)
                      else return (Left "Session Expired, please login again.")
                  )
                  undefined
          )
          undefined

  isValidTTL :: Integer -> Integer -> Bool
  isValidTTL now ttl = (now - ttl) < 0
