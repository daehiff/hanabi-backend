module UnitSpec where
import           Test.Hspec
-------------------------------------
import Unit.DatabaseTest (dataBaseTest)


spec :: Spec
spec = do
    dataBaseTest
  