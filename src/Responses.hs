{-# LANGUAGE OverloadedStrings #-}

module Responses where

import           Data.Aeson
import           Web.Spock                      ( SpockActionCtx
                                                , SpockCtxM
                                                , WebStateM
                                                )
import           Database.MongoDB               ( Pipe )

--------- Datatypes necessary for running configuring the app ---------

type App ctx = SpockCtxM ctx Pipe () AppConfig ()

type AppHandle ctx a = SpockActionCtx ctx Pipe () AppConfig a

type AppStateM sess = WebStateM Pipe sess AppConfig

data AppConfig = AppConfig {dbConf:: DBConf, port::Int, jwtSecret::String}

data DBConf = DBConf {hostUrl:: String,
                      dbUser::String,
                      dbPass::String,
                      dbName::String,
                      useReplica::Bool}
      deriving(Show)
-----------------------------------------------------------------------

errorJson :: ToJSON a => Int -> a -> Value
errorJson code message = object
  [ "result" .= String "failure"
  , "error" .= object ["code" .= code, "message" .= message]
  ]

sucessJson :: ToJSON a => Int -> a -> Value
sucessJson code message = object
  [ "error" .= object []
  , "success" .= object ["code" .= code, "message" .= message]
  ]


{-Below the error success codes are defined-}

sucessCode :: Int
sucessCode = 0

authError :: Int
authError = 1

loginError :: Int
loginError = 2

registrationError :: Int
registrationError = 3

lobbyNotFoundError :: Int
lobbyNotFoundError = 4

errorCreateLobby:: Int
errorCreateLobby = 5

errorJoinLobby :: Int
errorJoinLobby = 6

errorKickPlayer :: Int
errorKickPlayer = 7

errorLaunch :: Int
errorLaunch = 8

errorAdjustSettings :: Int
errorAdjustSettings = 9
