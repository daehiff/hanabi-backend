{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Utils where
import qualified Data.Text                     as T
import           Database.MongoDB hiding (Key)
import           Test.Hspec
import           Model
import Model.Utils (findObjects, insertObject, findById, run)
import           System.Environment             ( getEnv )

import           Init                           ( app )
import           Web.Spock                      ( spock
                                                , spockAsApp
                                                )
import           Network.Wai                    ( Middleware )
import           Web.Spock.Config


flushDB :: IO ()
flushDB  = dropDB >> return ()
  where
    dropDB = do
      db_name   <- (getEnv "DB_NAME")
      run $ dropDatabase $ T.pack db_name 


testApp :: IO Middleware
testApp = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  spock spockCfg app