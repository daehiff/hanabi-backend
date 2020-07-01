module Responses where

import           Data.Aeson


errorJson :: ToJSON a => Int -> a -> Value
errorJson code message = object
  [ "result" .= String "failure"
  , "error" .= object ["code" .= code, "message" .= message]
  ]

sucessJson :: ToJSON a => Int -> a -> Value
sucessJson code message = object
  [ "error" .= object []
  , "sucess" .= object ["code" .= code, "message" .= message]
  ]


{-Below the error sucess codes are defined-}

sucessCode:: Int
sucessCode = 0

authError :: Int
authError = 1

loginError :: Int
loginError = 2