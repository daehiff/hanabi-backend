module Controller.Utils where
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Aeson.Types               ( parseEither
                                                , Parser
                                                )
import           Data.Aeson              hiding ( json )

import           Data.Time.Clock
import           Data.Time.Clock.POSIX

parseBody :: ByteString -> (Object -> Parser b) -> Either String b
parseBody rawBodyStr parseStrat = do
  let (eResult) = (eitherDecode rawBodyStr) :: Either String Object
  case eResult of
    (Left  error ) -> (Left error)
    (Right result) -> (flip parseEither result $ parseStrat)

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime t =
  posixSecondsToUTCTime $ fromIntegral $ roundFn $ utcTimeToPOSIXSeconds t


roundFn :: RealFrac a => a -> Integer
roundFn = round

getCurretISOTime :: IO UTCTime
getCurretISOTime = do
  now <- getCurrentTime
  return (roundUTCTime now)
