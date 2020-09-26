{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}

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
import           Controller.Utils               ( parseBody )
import qualified Data.ByteString.Char8         as BS8
import           Data.HVect                     ( HVect
                                                , ListContains
                                                , findFirst
                                                )



{-
Creates a new session for the user and adds it to the database.
Returns the user that is loged in and the corresponding jwt
-}
logUserIn :: User -> AppHandle (HVect xs) (User, T.Text)
logUserIn user = do
  let (Key userId) = uid user
  session <- (insertObject Session { sid = NewKey, sessionUser = (userId) })
  let (Key sessionId) = sid session
  let logedInUser = user { sessions = (sessionId : (sessions user)) }
  (updateObject logedInUser)
  now <- liftIO _getNow
  let payload = SAP { user      = logedInUser { sessions = [] }
                    , sessionid = sessionId
                    , ttl       = now + 15 * 60 * 1000
                    }
  jwt <- sessionToJWT payload
  return (logedInUser, jwt)


{-
@api {post} {{base_url}}/auth/login Log user in
@apiName login 
@apiGroup Auth
@apiDescription logs the user in by its email and password 
@apiErrorExample {json} Sample Input:
{
    "email": "samplemail@provider.com",
    "password": "supersaveandsecure"
}

-}
loginHandle :: AppHandle (HVect xs) ()
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
{-   checkUserEmailExist:: Either String (String, String) -> ActionCtxT ctx (AppStateM sess) (Either String (String, String))
  checkUserEmailExist (Left error) = return (Left error)
  checkUserEmailExist (Right (email, password)) = do -}


  checkUser
    :: Either String (String, String)
    -> AppHandle (HVect xs) (Either String User)
  checkUser (Left  error            ) = return (Left error)
  checkUser (Right (email, password)) = do
    (eUser :: Maybe User) <- findObject ["email" =: val email]
    case eUser of
      Nothing     -> return (Left "User with this email does not exist.")
      (Just user) -> return (validateUser password user)

   where
    validateUser :: String -> User -> (Either String User)
    validateUser upw user =
      let pwHash = fromMaybe "" (password_hash user)
      in  if validatePassword (pack (pwHash)) (pack (upw ++ pwsalt user))
            then (Right user)
            else (Left "invalid Password.")

{-
@api {post} {{base_url}}/auth/login Register user 
@apiName register
@apiGroup Auth
@apiDescription Register a new User
@apiErrorExample {json} Sample Input:
{
    "email": "samplemail@provider.com",
    "password": "supersaveandsecure",
    "username": "sampleusername"
}
-}
registerHandle :: AppHandle (HVect xs) ()
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
  userCreated <- liftIO (createuser eReqData)
  userValid   <-
    checkUserEmailExist userCreated
    >>= checkUserUsernameExists
    >>= checkEmptyMailUsername
  euser <- storeUser userValid
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
    if password == ""
      then return (Left "Password may not be empty")
      else do
        mSalt <- liftIO $ genSaltUsingPolicy fastBcryptHashingPolicy
        if mSalt == Nothing
          then return (Left "internal Server error")
          else do
            let (Just salt) = mSalt
            let pwSalt      = password ++ (BS8.unpack salt)
            passwordHash <- liftIO
              ((hashPasswordUsingPolicy fastBcryptHashingPolicy (pack pwSalt)))
            let newUser = User
                  { uid           = NewKey
                  , email         = email
                  , password_hash = (fmap unpackStrict8 passwordHash)
                  , username      = username
                  , sessions      = []
                  , pwsalt        = BS8.unpack salt
                  }
            return (Right newUser)

  checkEmptyMailUsername
    :: Either String User -> AppHandle (HVect xs) (Either String User)
  checkEmptyMailUsername (Left  error) = return (Left error)
  checkEmptyMailUsername (Right user ) = do
    if (email user) == "" || (username user) == ""
      then return (Left "Email or username empty!")
      else return (Right user)

  checkUserEmailExist
    :: Either String User -> AppHandle (HVect xs) (Either String User)
  checkUserEmailExist (Left  error) = return (Left error)
  checkUserEmailExist (Right user ) = do
    (existingUser :: Maybe User) <- (findObject ["email" =: val (email user)])
    case existingUser of
      (Just user) -> return (Left "user with this email already exists.")
      (Nothing  ) -> return (Right user)

  checkUserUsernameExists
    :: Either String User -> AppHandle (HVect xs) (Either String User)
  checkUserUsernameExists (Left  error) = return (Left error)
  checkUserUsernameExists (Right user ) = do
    (existingUser :: Maybe User) <-
      (findObject ["username" =: val (username user)])
    case existingUser of
      (Just user) -> return (Left "Username is already taken.")
      (Nothing  ) -> return (Right user)

  storeUser :: Either String User -> AppHandle (HVect xs) (Either String User)
  storeUser (Left  error) = return (Left error)
  storeUser (Right user ) = do
    insertedUser <- insertObject user
    return (Right insertedUser)


getUser :: (ListContains n User xs) => AppHandle (HVect xs) ()
getUser = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  let (Key _uid)   = uid user
  json $ sucessJson sucessCode user

getOtherUser :: String -> AppHandle (HVect xs) ()
getOtherUser id = do
  (mUser :: Maybe User) <- (findById id)
  let eUser = parseUser mUser
  case eUser of
    (Left error) -> do
      setStatus badRequest400
      json (errorJson userNotFoundError error)
    (Right user) -> do
      json (sucessJson sucessCode user)
 where

parseUser :: Maybe User -> (Either String User)
parseUser Nothing = (Left "User does not exist")
parseUser (Just user) = let nUser = user { sessions = [] } in (Right nUser)


