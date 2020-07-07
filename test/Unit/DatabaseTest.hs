{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unit.DatabaseTest where
import qualified Data.Text                     as T
import           Database.MongoDB hiding (Key)
import           Test.Hspec
import           Model
import Model.Utils (findObjects, insertObject, findById, run, ObjectKey(..))

import Utils (flushDB)

-- whats supposed to be executed before each unit test
beforeEach = do
  flushDB
  return ()

-- whats supposed to be executed after each unit test
afterEach = do
  flushDB
  return ()

dataBaseTest = (before_ beforeEach) $ (after_ afterEach) $ do
  describe "database" $ do
    it "stores cards correctly" $ do
      cardInserted <-
        (insertObject Card { cid = NewKey, number = 1, color = Red })
      let (Key id) = cid cardInserted
      (Just cardFound) <- (findById (id) :: IO (Maybe Card))
      cardFound `shouldBe` cardInserted
    it "can filter output correctly" $ do
      cardInserted0 <-
        (insertObject Card { cid = NewKey, number = 1, color = Green })
      cardInserted1 <-
        (insertObject Card { cid = NewKey, number = 2, color = Green })
      cardInserted2 <-
        (insertObject Card { cid = NewKey, number = 3, color = Green })
      let cardsIns = [cardInserted0, cardInserted1, cardInserted2]
      (cardsFoundM) <- (findObjects [] [])
      let cardsFound = [ x | (Just x) <- cardsFoundM ]
      cardsIns `shouldBe` cardsFound
    it "can sort elements" $ do
      -- insert some cards in DB
      cardInserted0 <-
        (insertObject Card { cid = NewKey, number = 1, color = Green })
      cardInserted1 <-
        (insertObject Card { cid = NewKey, number = 2, color = Green })
      cardInserted2 <-
        (insertObject Card { cid = NewKey, number = 3, color = Green })

      -- check ascending order
      let cardsIns = [cardInserted0, cardInserted1, cardInserted2]
      (cardsFoundM) <- (findObjects [] ["number" := (val (1 :: Int))])
      let cardsFound = [ x | (Just x) <- cardsFoundM ]
      length (cardsIns) `shouldBe` 3
      length (cardsFound) `shouldBe` 3
      let out = [ x == y | (x, y) <- zip cardsIns cardsFound ]
      out `shouldBe` [True, True, True]
      (cardsFoundM) <- (findObjects [] ["number" := (val (-1 :: Int))])

      -- check descending order
      let cardsIns   = [cardInserted2, cardInserted1, cardInserted0]
      let cardsFound = [ x | (Just x) <- cardsFoundM ]
      length (cardsIns) `shouldBe` 3
      length (cardsFound) `shouldBe` 3

      let out = [ x == y | (x, y) <- zip cardsIns cardsFound ]
      out `shouldBe` [True, True, True]