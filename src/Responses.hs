module Responses where

import           Data.Aeson


errorJson :: Int -> String -> Value
errorJson code message = object
  [ "result" .= String "failure"
  , "error" .= object ["code" .= code, "message" .= message]
  ]

sucessJson :: Int -> String -> Value
sucessJson code message = object
  [ "error" .= object []
  , "sucess" .= object ["code" .= code, "message" .= message]
  ]



{-Below the error sucess codes are defined-}

authError :: Int
authError = 1

loginError :: Int
loginError = 2