{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Chat where

import           Web.Spock
import           Web.Spock.Config
import           Responses
import           Network.HTTP.Types             ( badRequest400 )
import           Data.Text               hiding ( unfoldr
                                                , take
                                                , lines
                                                , length
                                                , map
                                                )
import qualified Data.Text                     as T
import           Data.List                      ( unfoldr )
import           Data.Bson                      ( val
                                                , (=:)
                                                )
import           Data.HVect              hiding ( length
                                                , (!!)
                                                )
import           Data.Time.Clock
import           Model                          ( User(..)
                                                , Lobby(..)
                                                , Chat(..)
                                                , Message(..)
                                                )
import           Model.Utils
import           Control.Monad.Trans
import           Controller.Chat               ( findChatById
                                                )
import           Controller.Utils               ( parseBody )
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                , ByteString
                                                )

import           Data.Aeson                     ( encode )
import           Data.Aeson.Types               ( Parser
                                                , (.:)
                                                )
import           Database.MongoDB               ( Pipe )
import           Control.Monad.Trans.Reader     ( ReaderT )

import           Controller.Utils               ( getCurretISOTime )

import           Data.Time.ISO8601              ( formatISO8601 )


{-
@api {post} {{base_url}}/chat/send Send message 
@apiName send
@apiGroup Chat
@apiHeader {String} auth Users auth Token.
@apiDescription Send a message in chat
@apiErrorExample {json} Sample Input:
{
    "chatID": "5f4f61486f74963fa8000000",
    "messages": [
                {
                    "sender": "5f3f92216f74961968000000",
                    "message": "adddadad",
                    "timestamp": "2020-09-02T09:17:14Z"
                }
                ]
}
-}
handleSendMessage
  :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
handleSendMessage _chatID = do
  rawBodyStr <- fromStrict <$> body
  let eSenderMessage = parseBody
        rawBodyStr
        (\obj -> do
          isPublic <- (obj .: "message") :: Parser String
          return isPublic
        )

  eChat <- buildMessage eSenderMessage >>= findUpdateChat _chatID

  case (eChat) of
    (Left error) -> do
      setStatus badRequest400
      json $ errorJson errorJoinLobby error
    (Right chat) -> do
      json $ sucessJson sucessCode chat



buildMessage
  :: (ListContains n User xs)
  => (Either String String)
  -> AppHandle (HVect xs) (Either String Message)
buildMessage (Left  error  ) = return (Left error)
buildMessage (Right message) = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  let (Key _id)    = uid user
  currentTimeStamp <- liftIO getCurretISOTime
  return (Right (Message message currentTimeStamp _id))

-- Todo: CHeck if player is in lobby with chatId

findUpdateChat
  :: String
  -> Either String Message
  -> AppHandle (HVect xs) (Either String Chat)
findUpdateChat _       (Left  error  ) = return (Left error)
findUpdateChat _chatID (Right message) = do
  (mChat :: Maybe Chat) <- findById _chatID
  case mChat of
    Nothing     -> return (Left ("Chat with Id " ++ _chatID ++ " not found."))
    (Just chat) -> do
      let newChat = chat { messages = message : (messages chat) }
      updateObject newChat
      return (Right newChat)


{-
@api {get} {{base_url}}/chat/status Send message 
@apiName status
@apiGroup Chat
@apiHeader {String} auth Users auth Token.
@apiDescription Get status with ChatID
@apiErrorExample {json} Sample Input:
{
    "chatID": "5f4f61486f74963fa8000000",
    "messages": [
                {
                    "sender": "5f3f92216f74961968000000",
                    "message": "adddadad",
                    "timestamp": "2020-09-02T09:17:14Z"
                }
                ]
}
-}

getChatStatus :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
getChatStatus chatId = do
  oldCtx <- getContext
  let user :: User = (findFirst oldCtx)
  let (Key userId) = uid user
  eChat <- findChatById chatId
  case eChat of
    (Left  error) -> json $ errorJson errorChat error
    (Right chat) -> json $ sucessJson sucessCode chat


{--
chat :: IO ()
chat $ do 
    _ <- printf "Start Chat and listening on port 8080\n"
    port <- findPort
    chan <- NewTChanIO
    bracket (listenOn PortNumber port)) () ()
    return ()

findPort :: IO PortNumber
findPort = do 
    chatport <- lookupEnv "CHAT_SERVER_PORT"
    let port = 
        case chatport of
            Nothing -> 8080
            Just str -> fromnIntegral (read str :: Int)
    return port

--}
