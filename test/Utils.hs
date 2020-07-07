{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Utils where
import qualified Data.Text                     as T
import           Database.MongoDB hiding (Key)
import           Test.Hspec
import           Model
import Model.Utils (findObjects, insertObject, findById, run)
import           System.Environment             ( getEnv )

flushDB :: IO ()
flushDB  = dropDB >> return ()
  where
    dropDB = do
      db_name   <- (getEnv "DB_NAME")
      run $ dropDatabase $ T.pack db_name 