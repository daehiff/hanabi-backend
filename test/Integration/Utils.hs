{-# LANGUAGE OverloadedStrings  #-}

module Integration.Utils
  ( errorResponse
  , sucessResponse
  , unwrapJWT
  )
where
import           Test.Hspec
import           Test.Hspec.Wai
import           Data.ByteString.Lazy.Internal  ( ByteString )
import qualified Data.ByteString.Internal      as BIB
import           Data.Aeson                     ( decode
                                                , encode
                                                )
import           Data.Aeson.Types               ( ToJSON )

import           Network.HTTP.Types.Header
import           Responses
import           Data.CaseInsensitive           ( CI(..) )
import qualified Data.ByteString.Char8         as BS

--sucessResponse :: ToJSON a => Int -> Int -> a ResponseMatcher
--checkErrorResponse httpStatus internalCode msg = 

errorResponse :: ToJSON a => Int -> Int -> a -> ResponseMatcher
errorResponse httpStatus internalCode msg = ResponseMatcher
  { matchStatus  = httpStatus
  , matchBody    = MatchBody (checkErrorResp internalCode msg)
  , matchHeaders = []
  }

sucessResponse :: ToJSON a => Int -> Int -> a -> ResponseMatcher
sucessResponse httpStatus internalCode msg = ResponseMatcher
  { matchStatus  = httpStatus
  , matchBody    = MatchBody (checkResp internalCode msg)
  , matchHeaders = [MatchHeader (checkHeaderPresent "auth")]
  }

checkHeaderPresent :: CI BS.ByteString -> [Header] -> Body -> Maybe String
checkHeaderPresent headerName headers _ =
  let mhrdVal = lookup headerName headers
  in  case mhrdVal of
        Nothing ->
          (Just ("Header: " ++ (show headerName) ++ "was not present:\n"))
        (Just hrdVal) -> Nothing




checkResp :: ToJSON a => Int -> a -> [Header] -> Body -> Maybe String
checkResp code expMsg _ body =
  let jsonBody_ = (encode $ sucessJson code (expMsg))
  in  if body == jsonBody_
        then Nothing
        else
          (Just
            (  "Error Matching Bodys. \nExpected: "
            ++ (show jsonBody_)
            ++ "\nReceived: "
            ++ (show body)
            )
          )


checkErrorResp :: ToJSON a => Int -> a -> [Header] -> Body -> Maybe String
checkErrorResp code expMsg _ body =
  let jsonBody = (encode $ errorJson code (expMsg))
  in  if body == jsonBody
        then Nothing
        else
          (Just
            (  "Error Matching Bodys. \nExpected: "
            ++ (show jsonBody)
            ++ "\nReceived: "
            ++ (show body)
            )
          )

unwrapJWT :: Maybe BIB.ByteString -> BIB.ByteString
unwrapJWT Nothing    = ""
unwrapJWT (Just jwt) = jwt

