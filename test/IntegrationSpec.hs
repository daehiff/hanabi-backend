module IntegrationSpec where

import Test.Hspec

main :: IO ()
main = hspec spec


-- TODO implement API calls to create one match
spec :: Spec
spec =
  describe "the universe" $
  it "behaves the way we expect it to" $ do
    1 `shouldBe` 1
