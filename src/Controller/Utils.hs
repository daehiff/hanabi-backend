module Controller.Utils where
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Aeson.Types               ( parseEither
                                                , Parser
                                                )
import           Data.Aeson              hiding ( json )



parseBody :: ByteString -> (Object -> Parser b) -> Either String b
parseBody rawBodyStr parseStrat = do
  let (eResult) = (eitherDecode rawBodyStr) :: Either String Object
  case eResult of
    (Left  error ) -> (Left error)
    (Right result) -> (flip parseEither result $ parseStrat)
