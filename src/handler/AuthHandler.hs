
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
import           Model                          ( findObject
                                                , User
                                                , uid
                                                , password_hash
                                                )
import Data.Bson (val, (=:))
import Crypto.BCrypt
import Data.ByteString.Char8 (pack)
import Responses
import Data.Maybe (fromMaybe)

loginHandler :: MonadIO m => ActionCtxT ctx m b
loginHandler = do
  rawBodyStr <- fromStrict <$> body
  let result = parseBody rawBodyStr
  userData <- liftIO (lookupUser result)
  case userData of 
    (Left error) -> json (errorJson loginError error)
    (Right userData)     -> json userData


parseBody :: ByteString -> Either String (String, String)
parseBody rawBodyStr = do
  let (eResult) = (eitherDecode rawBodyStr) :: Either String Object
  case eResult of
    (Left error) -> (Left error)
    (Right result) ->
      ( flip parseEither result
      $ (\obj -> do
          email    <- (obj .: "email") :: Parser String
          password <- (obj .: "password") :: Parser String
          return (email, password)
        )
      )

lookupUser :: Either String (String, String) -> IO (Either String User)
lookupUser (Left  error            ) = return (Left error)
lookupUser (Right (email, password)) = do
  eUser <- findObject ["email" =: val email] :: IO (Maybe User)
  case eUser of 
    Nothing -> return (Left "User not avaiable")
    (Just user) -> return (checkPassword password user ) 



checkPassword:: String -> User -> (Either String User)
checkPassword upw user = let pwHash = fromMaybe "" (password_hash user) in if validatePassword (pack (pwHash)) (pack upw) then (Right user) else (Left "invalid password")
