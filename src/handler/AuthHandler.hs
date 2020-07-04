{-# LANGUAGE OverloadedStrings #-}


module AuthHandler where

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Trans
import           Data.IORef
import           Data.Aeson              hiding ( json )
import           Data.Aeson.Types               ( parseEither
                                                , parseMaybe
                                                , Parser
                                                )

import qualified Data.Text                     as T
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                , ByteString
                                                )
import           Model
import           ModelUtils                     ( findObject
                                                , insertObject
                                                , updateObject
                                                , findObjects
                                                , findById
                                                )

import           Data.Bson                      ( val
                                                , (=:)
                                                )
import           Crypto.BCrypt
import           Data.ByteString.Char8          ( pack )
import           Responses
import           Data.Maybe                     ( fromMaybe )
import           Hooks
import           Control.Lens.Internal.ByteString
                                                ( unpackStrict8 )
import           Network.HTTP.Types             ( badRequest400 )
import           BSONExtention                  ( ObjectKey(..) )
{-
Creates a new session for the user and adds it to the database.
Returns the user that is loged in and the corresponding jwt
-}
logUserIn :: User -> IO (User, T.Text)
logUserIn user = do
  let (Key userId) = uid user
  session <- (insertObject Session { sid = NewKey, sessionUser = (userId) })
  let (Key sessionId) = sid session
  let logedInUser = user { sessions = (sessionId : (sessions user)) }
  liftIO (updateObject logedInUser)
  now <- liftIO _getNow
  let payload = SAP { user      = logedInUser
                    , sessionid = sessionId
                    , ttl       = now + 15 * 60 * 1000
                    }
  jwt <- liftIO $ sessionToJWT payload
  return (logedInUser, jwt)

{-
TODO add api doc here
-}
loginHandle :: MonadIO m => ActionCtxT ctx m b
loginHandle = do
  rawBodyStr <- fromStrict <$> body
  let eEmailPw = parseBody
        rawBodyStr
        (\obj -> do
          email    <- (obj .: "email") :: Parser String
          password <- (obj .: "password") :: Parser String
          return (email, password)
        )
  eUserData <- liftIO (checkUser eEmailPw)
  case eUserData of
    (Left error) ->
      (\_ -> do
          setStatus badRequest400
          json (errorJson loginError error)
        )
        undefined
    (Right user) ->
      (\_ -> do
          (logedInUser, jwt) <- liftIO $ logUserIn user
          setHeader "auth" jwt
          json (sucessJson sucessCode logedInUser)
        )
        undefined
 where
  checkUser :: Either String (String, String) -> IO (Either String User)
  checkUser (Left  error            ) = return (Left error)
  checkUser (Right (email, password)) = do
    eUser <- findObject ["email" =: val email] :: IO (Maybe User)
    case eUser of
      Nothing     -> return (Left "User not avaiable")
      (Just user) -> return (validateUser password user)

   where
    validateUser :: String -> User -> (Either String User)
    validateUser upw user =
      let pwHash = fromMaybe "" (password_hash user)
      in  if validatePassword (pack (pwHash)) (pack upw)
            then (Right user)
            else (Left "invalid password")

{-
TODO add apidoc here
-}
registerHandle :: MonadIO m => ActionCtxT ctx m b
registerHandle = do
  rawBodyStr <- fromStrict <$> body
  let eReqData = parseBody
        rawBodyStr
        (\obj -> do
          email    <- (obj .: "email") :: Parser String
          password <- (obj .: "password") :: Parser String
          username <- (obj .: "username") :: Parser String
          return (email, username, password)
        )
  userCreated    <- liftIO (createuser eReqData)
  emailValidated <- liftIO $ validateEmail userCreated
  userValid      <- liftIO $ validateUsername emailValidated
  euser          <- liftIO $ storeUser userValid
  case euser of
    (Left error) ->
      (\_ -> do
          setStatus badRequest400
          json (errorJson registrationError error)
        )
        undefined
    (Right user) ->
      (\_ -> do
          (logedInUser, jwt) <- liftIO $ logUserIn user
          setHeader "auth" jwt
          json (sucessJson sucessCode logedInUser)
        )
        undefined
 where
  createuser
    :: Either String (String, String, String) -> IO (Either String User)
  createuser (Left  error                      ) = return (Left error)
  createuser (Right (email, username, password)) = do
    passwordHash <- liftIO
      ((hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)))
    let newUser = User { uid           = NewKey
                       , email         = email
                       , password_hash = (fmap unpackStrict8 passwordHash)
                       , username      = username
                       , sessions      = []
                       }
    return (Right newUser)

  validateEmail :: Either String User -> IO (Either String User)
  validateEmail (Left  error) = return (Left error)
  validateEmail (Right user ) = do
    existingUser <-
      liftIO (findObject ["email" =: val (email user)]) :: IO (Maybe User)
    case existingUser of
      (Just user) -> return (Left "user with this email already exists.")
      (Nothing  ) -> return (Right user)

  validateUsername :: Either String User -> IO (Either String User)
  validateUsername (Left  error) = return (Left error)
  validateUsername (Right user ) = do
    existingUser <-
      liftIO (findObject ["username" =: val (username user)]) :: IO (Maybe User)
    case existingUser of
      (Just user) -> return (Left "Username is already taken.")
      (Nothing  ) -> return (Right user)

  storeUser :: Either String User -> IO (Either String User)
  storeUser (Left  error) = return (Left error)
  storeUser (Right user ) = do
    insertedUser <- insertObject user
    return (Right insertedUser)



{-
Parse a json Body by a custom set strategy defined by a function that maps the object towards the custom datatype
-}
parseBody :: ByteString -> (Object -> Parser b) -> Either String b
parseBody rawBodyStr parseStrat = do
  let (eResult) = (eitherDecode rawBodyStr) :: Either String Object
  case eResult of
    (Left  error ) -> (Left error)
    (Right result) -> (flip parseEither result $ parseStrat)

