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
import           Data.Text.Encoding             ( encodeUtf8, decodeUtf8 )
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )
import           Data.Time.Clock.POSIX          ( getPOSIXTime
                                                , POSIXTime
                                                )
import           Data.ByteString.Internal       ( ByteString )

import           Network.HTTP.Types             ( badRequest400 )
import qualified Data.ByteString.Char8         as BS8
import           Responses
import           Database.MongoDB               ( Pipe )
import           Control.Monad.Trans.Reader     ( ReaderT )

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
initHook :: ActionCtxT () (WebStateM Pipe () AppConfig) (HVect '[])
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
  => ActionCtxT (HVect xs) (WebStateM Pipe p AppConfig) (HVect xs)
updateJWTHook = do
  oldCtx <- getContext
  let oldPayload :: SAP = (findFirst oldCtx)
  now <- liftIO _getNow
  let newTTL  = now + 15 * 60 * 1000 
  let payload = oldPayload { ttl = newTTL }
  jwt <- sessionToJWT payload
  setHeader "auth" jwt
  return oldCtx


sessionToJWT
  :: (HasSpock m, MonadIO m, SpockState m ~ AppConfig) => SAP -> m T.Text
sessionToJWT payload = do
  jwtSecret <-
    getState
      >>= (\appCfg -> do
            liftIO $ return $ jwtSecret appCfg
          )
  let (Right jwt) = hmacEncode HS384 (BS8.pack jwtSecret) (toStrict (encode payload))
  return (decodeUtf8 (unJwt jwt))

{-
Auth hook that checks the users JWT
-}
authHook
  :: ActionCtxT
       (HVect xs)
       (WebStateM Pipe p AppConfig)
       (HVect (User ': SAP ': xs))
authHook = do
  oldCtx             <- getContext
  jwt                <- (header "auth")
  rawPayload         <- getRawPayload jwt
  unValidatedPayload <- liftIO $ convertPayloadToSAP rawPayload
  ePayload           <- validatePayload unValidatedPayload
  case ePayload of
    (Left errorMsg) -> do
      setStatus badRequest400
      json $ errorJson authError errorMsg
    (Right payload) -> return (user payload :&: payload :&: oldCtx)
 where
  getRawPayload Nothing    = return (Left "Header: `auth` is missing")
  getRawPayload (Just jwt) = do
    jwtSecret <-
      getState
        >>= (\appCfg -> do
              liftIO $ return $ jwtSecret appCfg
            )
    let eitherDecoded = hmacDecode (BS8.pack jwtSecret) (encodeUtf8 jwt)
    case eitherDecoded of
      (Left error) -> return (Left ("Unable to parse JWT: " ++ show error))
      (Right (_, payload)) -> return (Right payload)

  --convertPayloadToSAP :: Either String ByteString -> IO (Either String SAP)
  convertPayloadToSAP (Left  error     ) = return (Left error)
  convertPayloadToSAP (Right rawPayload) = do
    let payload = (decode (fromStrict rawPayload)) :: Maybe SAP
    case payload of
      Nothing -> return (Left "Error Parsing Payload, try to login again")
      (Just payload) -> return (Right payload)

  validatePayload
    :: Either String SAP
    -> ActionCtxT (HVect xs) (WebStateM Pipe p AppConfig) (Either String SAP)
  validatePayload (Left  error  ) = return (Left error)
  validatePayload (Right payload) = do
    now <- liftIO $ _getNow
    let userTtl   = ttl payload
    let tokenUser = user payload
    if (isValidTTL now userTtl)
      then return (Right payload)
      else do
        let (Key userId) = (uid $ user payload)
        (mUser :: Maybe User) <- findById userId
        if mUser == Nothing
          then return (Left "Could not verify user, please login again.")
          else do
            let (Just user) = mUser
            let newPayload  = payload { user = user }
            if (sessionid newPayload) `elem` (sessions user)
              then return (Right newPayload { user = user {sessions=[]}})
              else return (Left "Session Expired, please login again.")

  isValidTTL :: Integer -> Integer -> Bool
  isValidTTL now ttl = (now - ttl) < 0
