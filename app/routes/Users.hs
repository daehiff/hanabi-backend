{-# LANGUAGE OverloadedStrings #-}

module Users where

import           Web.Spock               hiding ( runQuery )

import           Control.Monad.Trans

import qualified Data.Text                     as T
import           Text.Read                      ( readMaybe )



userRoute :: SpockCtxM ctx conn sess st ()
userRoute = do
  get "user" getuserHandler
  get ("user" <//> var) $ getUserById


getuserHandler :: MonadIO m => ActionCtxT ctx m a
getuserHandler = text "userhandler"

getUserById :: MonadIO m => String -> ActionCtxT ctx m a
getUserById idString =
  let mId = readMaybe (idString) :: Maybe Int
  in  case mId of
        Nothing   -> text "Error id must be Integer"
        (Just id) -> text ("UserId: " <> (T.pack (show id)) <> " welcome! :)")

