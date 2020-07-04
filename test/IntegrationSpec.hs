module IntegrationSpec where
import qualified Data.Text                     as T
import           Database.MongoDB        hiding ( Key )
import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai                    ( Middleware )
import           Model
import           ModelUtils                     ( findObjects
                                                , insertObject
                                                , findById
                                                , run
                                                )
import           BSONExtention
import           Utils                          ( flushDB )
import           Data.ByteString.Lazy.Internal  ( ByteString )
import           Init                           ( app )
import           Web.Spock                      ( spock
                                                , spockAsApp
                                                )
import           Web.Spock.Config

import Integration.AuthHandle (authHandleTest)

beforeAll = do
  flushDB

afterAll = do
  flushDB

testApp :: IO Middleware
testApp = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  spock spockCfg app

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (spockAsApp testApp) $ do
  authHandleTest