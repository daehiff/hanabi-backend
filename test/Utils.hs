{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Utils where
import qualified Data.Text                     as T
import           Database.MongoDB hiding (Key)
import           Test.Hspec
import           Model
import           System.Environment             ( getEnv )


run act = do
  host_addr <- (getEnv "DB_ADDR")
  db_name   <- (getEnv "DB_NAME")
  pipe      <- connect $ host host_addr
  access pipe master (T.pack db_name) act

flushDB :: IO ()
flushDB  = dropDB >> return ()
  where
    dropDB = do
      db_name   <- (getEnv "DB_NAME")
      run $ dropDatabase $ T.pack db_name 

