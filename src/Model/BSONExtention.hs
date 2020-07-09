{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.BSONExtention where

import           GHC.Generics

import           Control.Applicative
import           Control.Monad

import qualified Data.Bson                     as BSON
                                                ( lookup )
import           Data.Bson
import qualified Data.Text                     as T
                                                ( pack, unpack )
import           Data.Typeable

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(parseJSON)
                                                )
import           Data.Aeson.Types               ( Parser(..) )
import qualified Data.Aeson                    as AE

data ObjectKey =  NewKey | Key String
        deriving (Generic, Typeable, Show, Eq)

instance ToJSON ObjectKey where
  toJSON NewKey   = AE.Null
  toJSON (Key id) = toJSON id

instance FromJSON ObjectKey where
  parseJSON AE.Null = return NewKey
  parseJSON (AE.String id) = return (Key (T.unpack id))

constructorLabel :: Label
constructorLabel = T.pack "_co"

keyLabel :: Label
keyLabel = T.pack "_id"

data Test = Test{tid::ObjectKey, name::String} deriving (Show, Generic, Typeable)

instance ToBSON Test
instance FromBSON Test

class ToBSON a where
    toBSON :: a -> Document

    default toBSON :: (Generic a, GToBSON (Rep a)) => a -> Document
    toBSON a = genericToBSON (from a)

class GToBSON f where
    genericToBSON :: f a -> Document

instance GToBSON U1 where
    genericToBSON U1 = []

instance (GToBSON a, GToBSON b) => GToBSON (a :*: b) where
  genericToBSON (x :*: y) = genericToBSON x ++ genericToBSON y

instance (GToBSON a, GToBSON b) => GToBSON (a :+: b) where
  genericToBSON (L1 x) = genericToBSON x
  genericToBSON (R1 x) = genericToBSON x

instance (GToBSON a) => GToBSON (D1 c a) where
  genericToBSON (M1 x) = genericToBSON x

instance (GToBSON a, Constructor c) => GToBSON (C1 c a) where
  genericToBSON c@(M1 x) = genericToBSON x ++ [constructorLabel =: conName c]

instance (Val a, Selector s) => GToBSON (S1 s (K1 i a)) where
  --genericToBSON (M1 (K1 (Key id))) = [keyLabel =: id]
  genericToBSON s@(M1 (K1 x)) = [T.pack (selName s) =: x]

instance {-# OVERLAPPING #-} (Selector s) => GToBSON (S1 s (K1 i ObjectKey)) where
  genericToBSON (M1 (K1 (Key id))) = [keyLabel =: (ObjId (read id))]
  genericToBSON _                  = []
instance (ToBSON a) => GToBSON (K1 i a) where
  genericToBSON (K1 x) = toBSON x


class FromBSON a where
    fromBSON :: Document -> Maybe a

    default fromBSON :: (Generic a, GFromBSON (Rep a)) => Document -> Maybe a
    fromBSON doc = maybe Nothing (Just . to) (genericFromBSON doc)

class GFromBSON f where
    genericFromBSON :: Document -> Maybe (f a)

instance GFromBSON U1 where
  genericFromBSON _ = Just U1

instance (GFromBSON a, GFromBSON b) => GFromBSON (a :*: b) where
  genericFromBSON doc = do
    x <- genericFromBSON doc
    y <- genericFromBSON doc
    return $ x :*: y

instance (GFromBSON a, GFromBSON b) => GFromBSON (a :+: b) where
  genericFromBSON doc = left `mplus` right
   where
    left  = L1 <$> genericFromBSON doc
    right = R1 <$> genericFromBSON doc

instance (GFromBSON a, Constructor c) => GFromBSON (C1 c a) where
  genericFromBSON doc = do
    cname <- BSON.lookup constructorLabel doc
    if cname == conName (undefined :: M1 C c a r)
      then M1 <$> genericFromBSON doc
      else Nothing

instance (GFromBSON a) => GFromBSON (M1 D c a) where
  genericFromBSON doc = M1 <$> genericFromBSON doc

instance (Val a, Selector s) => GFromBSON (S1 s (K1 i a)) where
  genericFromBSON doc = M1 . K1 <$> BSON.lookup sname doc
    where sname = T.pack . selName $ (undefined :: S1 s (K1 i a) r)

instance {-# OVERLAPPING #-} (Selector s) => GFromBSON (S1 s (K1 i ObjectKey)) where
  genericFromBSON doc = do
    let mid = (BSON.lookup keyLabel doc) :: Maybe ObjectId
    case mid of
      Nothing    -> M1 . K1 <$> Just NewKey
      (Just _id) -> M1 . K1 <$> Just (Key (show _id))
