{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unit.DatabaseTest where
import qualified Data.Text                     as T
import           Database.MongoDB        hiding ( Key )
import           Test.Hspec
import           Model
import           Model.Utils                    ( findObjects
                                                , insertObject
                                                , findById
                                                , run
                                                , ObjectKey(..)
                                                )

import           Utils                          ( flushDB )

beforeEach = do
  flushDB
  return ()

afterEach = do
  flushDB
  return ()

insertCards = do
  cardInserted0 <-
    (insertObject Card { cid = NewKey, number = 1, color = Green })
  cardInserted1 <-
    (insertObject Card { cid = NewKey, number = 2, color = Green })
  cardInserted2 <-
    (insertObject Card { cid = NewKey, number = 3, color = Green })
  return (cardInserted0, cardInserted1, cardInserted2)

dataBaseTest = (before_ beforeEach) $ (after_ afterEach) $ do
  describe "database" $ do
    it "stores cards correctly" $ do
      cardInserted <-
        (insertObject Card { cid = NewKey, number = 1, color = Red })
      let (Key id) = cid cardInserted
      (Just cardFound) <- (findById (id) :: IO (Maybe Card))
      cardFound `shouldBe` cardInserted

      let notUidString = "sadfs"
      dummyCard <- findById notUidString :: IO (Maybe Card)
      dummyCard `shouldBe` Nothing
    it "can filter output correctly" $ do
      (cardInserted0, cardInserted1, cardInserted2) <- insertCards
      let cardsIns = [cardInserted0, cardInserted1, cardInserted2]
      (cardsFoundM) <- (findObjects [] [])
      let cardsFound = [ x | (Just x) <- cardsFoundM ]
      cardsIns `shouldBe` cardsFound
    it "can sort elements" $ do
      -- insert some cards in DB
      (cardInserted0, cardInserted1, cardInserted2) <- insertCards
      
      -- check ascending order
      let cardsIns = [cardInserted0, cardInserted1, cardInserted2]
      (cardsFoundM) <- (findObjects [] ["number" := (val (1 :: Int))])
      let cardsFound = [ x | (Just x) <- cardsFoundM ]
      length (cardsIns) `shouldBe` 3
      length (cardsFound) `shouldBe` 3
      let out = [ x == y | (x, y) <- zip cardsIns cardsFound ]
      out `shouldBe` [True, True, True]
      
      -- check descending order
      (cardsFoundM) <- (findObjects [] ["number" := (val (-1 :: Int))])
      let cardsIns   = [cardInserted2, cardInserted1, cardInserted0]
      let cardsFound = [ x | (Just x) <- cardsFoundM ]
      length (cardsIns) `shouldBe` 3
      length (cardsFound) `shouldBe` 3
      let out = [ x == y | (x, y) <- zip cardsIns cardsFound ]
      out `shouldBe` [True, True, True]
