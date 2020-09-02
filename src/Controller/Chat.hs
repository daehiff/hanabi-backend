{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Controller.Chat where -- no control

import           Model
import           Model.Utils
import           System.Random                  ( randomRIO )
import           Responses
import           Web.Spock                      ( ActionCtxT )
import           Data.HVect

findChatById :: String -> AppHandle (HVect xs) (Either String Chat)
findChatById chatId = do
  (mchat :: Maybe Chat) <- findById chatId
  case mchat of
    Nothing     -> return (Left "Chat not found")
    (Just chat) -> return (Right chat)
