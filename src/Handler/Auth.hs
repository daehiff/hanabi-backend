{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Handler.Auth where

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
import           Model.Utils                    ( findObject
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
import           Model.Utils
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Database.MongoDB               ( Pipe )
-- TODO generic secret for jwt

{-
Creates a new session for the user and adds it to the database.
Returns the user that is loged in and the corresponding jwt
-}
logUserIn
  :: (MonadTrans t, MonadIO (t (AppStateM sess)))
  => User
  -> t (AppStateM sess) (User, T.Text)
logUserIn user = do
  let (Key userId) = uid user
  session <- (insertObject Session { sid = NewKey, sessionUser = (userId) })
  let (Key sessionId) = sid session
  let logedInUser = user { sessions = (sessionId : (sessions user)) }
  (updateObject logedInUser)
  now <- liftIO _getNow
  let payload = SAP { user      = logedInUser
                    , sessionid = sessionId
                    , ttl       = now + 15 * 60 * 1000
                    }
  jwt <-  sessionToJWT payload
  return (logedInUser, jwt)

{-
TODO add api doc here
-}
loginHandle :: ActionCtxT ctx (AppStateM sess) b
loginHandle = do
  rawBodyStr <- fromStrict <$> body
  let eEmailPw = parseBody
        rawBodyStr
        (\obj -> do
          email    <- (obj .: "email") :: Parser String
          password <- (obj .: "password") :: Parser String
          return (email, password)
        )
  eUserData <- (checkUser eEmailPw)
  case eUserData of
    (Left error) -> do
      setStatus badRequest400
      json (errorJson loginError error)
    (Right user) -> do
      (logedInUser, jwt) <- logUserIn user
      setHeader "auth" jwt
      json (sucessJson sucessCode logedInUser)
 where
  checkUser :: Either String (String, String) -> ActionCtxT ctx (AppStateM sess) (Either String User)
  checkUser (Left  error            ) = return (Left error)
  checkUser (Right (email, password)) = do
    (eUser :: Maybe User ) <- findObject ["email" =: val email]
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
registerHandle :: ActionCtxT ctx (AppStateM sess) b
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
  emailValidated <- validateEmail userCreated
  userValid      <- validateUsername emailValidated
  euser          <- storeUser userValid
  case euser of
    (Left error) -> do
      setStatus badRequest400
      json (errorJson registrationError error)
    (Right user) -> do
      (logedInUser, jwt) <- logUserIn user
      setHeader "auth" jwt
      json (sucessJson sucessCode logedInUser)
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

  validateEmail :: Either String User -> ActionCtxT ctx (AppStateM sess) (Either String User)
  validateEmail (Left  error) = return (Left error)
  validateEmail (Right user ) = do
    (existingUser :: Maybe User) <- (findObject ["email" =: val (email user)]) 
    case existingUser of
      (Just user) -> return (Left "user with this email already exists.")
      (Nothing  ) -> return (Right user)

  validateUsername :: Either String User -> ActionCtxT ctx (AppStateM sess) (Either String User)
  validateUsername (Left  error) = return (Left error)
  validateUsername (Right user ) = do
    (existingUser :: Maybe User) <- (findObject ["username" =: val (username user)])
    case existingUser of
      (Just user) -> return (Left "Username is already taken.")
      (Nothing  ) -> return (Right user)

  storeUser :: Either String User -> ActionCtxT ctx (AppStateM sess)  (Either String User)
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

