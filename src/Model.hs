{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(parseJSON)
                                                , decode
                                                , encode
                                                )
import           Data.Aeson.Types        hiding ( String )
import           Database.MongoDB
import           GHC.Generics
import qualified Data.Text                     as T
import           System.Environment             ( getEnv )

import           BSONExtention                  ( ToBSON(..)
                                                , FromBSON(..)
                                                , ObjectKey(..)
                                                )
import qualified BSONExtention                 as BSE
import           ModelUtils                     ( MongoObject(..) )

data Color = Red | Blue | White | Yellow | Green | Rainbow
            deriving(Generic, Show, Eq , ToJSON, FromJSON, ToBSON, FromBSON)

map_color_string :: Color -> String
map_color_string Red     = "Red"
map_color_string Blue    = "Blue"
map_color_string White   = "White"
map_color_string Yellow  = "Yellow"
map_color_string Green   = "Green"
map_color_string Rainbow = "Rainbow"

map_string_color ("Red"    ) = Just Red
map_string_color ("Blue"   ) = Just Blue
map_string_color ("White"  ) = Just White
map_string_color ("Yellow" ) = Just Yellow
map_string_color ("Green"  ) = Just Green
map_string_color ("Rainbow") = Just Rainbow
map_string_color _           = Nothing


instance Val Color where
  val color = val (map_color_string color)
  cast' (String color) = map_string_color color
  cast' _              = Nothing

data Card = Card {cid::ObjectKey , color::Color, number::Int}

              deriving(Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

instance MongoObject Card where
  collection _ = "cards"

  insertId id card = card { cid = BSE.Key (show id) }


data User = User {uid::ObjectKey,username::String, email::String, password_hash:: Maybe String ,sessions::[String]}
                    deriving (Show, Generic, Eq, ToBSON, FromBSON)

instance ToJSON User where
  toJSON user = object
    [ "uid" .= uid user
    , "username" .= (username user)
    , "email" .= email user
    , "sessions" .= (toJSON (sessions user))
    ]

instance FromJSON User where
  parseJSON (Object v) = do
    uid      <- v .: "uid"
    username <- v .: "username"
    email    <- v .: "email"
    sessions <- v .: "sessions"
    return User { uid           = uid
                , username      = username
                , email         = email
                , password_hash = Nothing
                , sessions      = sessions
                }


instance MongoObject User where
  insertId id user = user { uid = BSE.Key (show id) }

  collection _ = "users"


data Session = Session {sid:: ObjectKey, sessionUser::String}
              deriving (Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)


instance MongoObject Session where
  collection _ = "sessions"

  insertId id session = session { sid = BSE.Key (show id) }



-- > encode Session{sid=NewKey, sessionUser="sadsfg"}
