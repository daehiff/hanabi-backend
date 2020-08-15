{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Model where

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(parseJSON)
                                                , decode
                                                , encode
                                                )
import           Data.Aeson.Types        hiding ( String
                                                , Key
                                                )
import           Database.MongoDB        hiding ( Key )
import           GHC.Generics
import qualified Data.Text                     as T
import           System.Environment             ( getEnv )


import           Model.Utils                    ( MongoObject(..)
                                                , ObjectKey(..)
                                                , FromBSON(..)
                                                , ToBSON(..)
                                                )
import           Data.Time.Clock
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

data Card = Card {cid::ObjectKey , color::Color, number::Int, hintColor::[Color], hintNumber:: Maybe Int}

              deriving(Generic, Show, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

instance MongoObject Card where
  collection _ = "cards"

  insertId id card = card { cid = Key (show id) }


data User = User {uid::ObjectKey,username::String, email::String, password_hash:: Maybe String ,sessions::[String], pwsalt::String}
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
                , pwsalt        = ""
                }


instance MongoObject User where
  insertId id user = user { uid = Key (show id) }

  collection _ = "users"


data Session = Session {sid:: ObjectKey, sessionUser::String}
              deriving (Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)


instance MongoObject Session where
  collection _ = "sessions"

  insertId id session = session { sid = Key (show id) }


data Lobby = Lobby {lid:: ObjectKey,
                    lobbyHost:: String,
                    player::[String],
                    kickedPlayer::[String],
                    created::UTCTime,
                    gameId::Maybe String,
                    salt::String,
                    public::Bool,
                    launched::Bool}
                    deriving (Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

instance MongoObject Lobby where
  collection _ = "lobbys"

  insertId id lobby = lobby { lid = Key (show id) }

data Settings = Settings {amtLives:: Int,
                          amtHints:: Int,
                          level:: Level,
                          isRainbow:: Bool}
                    deriving (Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data Level = Beginner | Easy | Middle | Hard
              deriving (Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

map_level_string :: Level -> String
map_level_string Beginner = "Beginner"
map_level_string Easy     = "Easy"
map_level_string Middle   = "Middle"
map_level_string Hard     = "Hard"

map_string_level ("Beginner") = Just Beginner
map_string_level ("Easy"    ) = Just Easy
map_string_level ("Middle"  ) = Just Middle
map_string_level ("Hard"    ) = Just Hard
map_string_level _            = Nothing

instance Val Level where
  val level = val (map_level_string level)
  cast' (String level) = map_string_level level
  cast' _              = Nothing


data Game = Game {gid:: ObjectKey,
                  currentPlayer:: String,
                  players:: [Player],
                  hints:: Int,
                  lives:: Int,
                  drawPile:: [String],
                  discardPile:: [String],
                  redPile:: Int,
                  greenPile:: Int,
                  bluePile:: Int,
                  yellowPile:: Int,
                  whitePile:: Int,
                  rainbowPile:: Int}
                  deriving(Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

instance MongoObject Game where
  collection _ = "games"

  insertId id game = game { gid = Key (show id) }

data Player = Player {playerId:: String,
                      name:: String,
                      cards:: [String],
                      explicitHints:: [(Either Color Int, String)]}
                      --implicitHints:: [(String, String)]}
                      deriving(Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

data Hint = Hint {hid:: ObjectKey,
                  red:: Maybe Bool,
                  blue:: Maybe Bool,
                  green:: Maybe Bool,
                  yellow:: Maybe Bool,
                  white:: Maybe Bool,
                  rainbow:: Maybe Bool,
                  one:: Maybe Bool,
                  two:: Maybe Bool,
                  three:: Maybe Bool,
                  four:: Maybe Bool,
                  five:: Maybe Bool}
                  deriving(Show, Generic, Eq, ToJSON, FromJSON, ToBSON, FromBSON)

instance MongoObject Hint where
  collection _ = "hints"

  insertId id hint = hint { hid = Key (show id) }



-- $> Hint {hid = NewKey, red = Just True, blue = Nothing, green = Nothing, yellow = Just True, white = Nothing, rainbow = Nothing, one = Just True, two = Nothing, three = Just False, four = Just False, five = Nothing}

