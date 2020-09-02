module Controller.Utils where
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Aeson.Types               ( parseEither
                                                , Parser
                                                )
import           Data.Aeson              hiding ( json )

import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Array.IO                  ( IOArray
                                                , newListArray
                                                , writeArray
                                                , readArray
                                                )
import           System.Random                  ( randomRIO )
import           Control.Monad                  ( forM )

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

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j  <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
 where
  n = length xs
  newArray :: Int -> [a] -> IO (IOArray Int a)
  newArray n xs = newListArray (1, n) xs
