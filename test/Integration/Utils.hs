module Integration.Utils
  ( errorResponse
  )
where
import           Test.Hspec
import           Test.Hspec.Wai
import           Data.ByteString.Lazy.Internal  ( ByteString )

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
  , matchBody    = MatchBody (createCheckErrorResponse internalCode msg)
  , matchHeaders = []
  }

checkHeaderPresent :: CI BS.ByteString -> [Header] -> Body -> Maybe String
checkHeaderPresent headerName headers _ =
  let mhrdVal = lookup headerName headers
  in  case mhrdVal of
        Nothing ->
          (Just ("Header: " ++ (show headerName) ++ "was not present:\n"))
        (Just hrdVal) -> Nothing

createCheckErrorResponse
  :: ToJSON a => Int -> a -> [Header] -> Body -> Maybe String
createCheckErrorResponse code expMsg _ body =
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
