{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Utils
  ( ObjectKey(..)
  , MongoObject(..)
  , FromBSON(..)
  , ToBSON(..)
  , DBConf(..)
  , run
  , run'
  , setupDB
  )
where

import           Data.Aeson                     ( ToJSON()
                                                , FromJSON(parseJSON)
                                                )
import           Database.MongoDB        hiding ( lookup )
import qualified Data.Text                     as T
import           System.Environment             ( getEnv
                                                , lookupEnv
                                                )
import           Model.BSONExtention
import           Data.Maybe                     ( fromMaybe )

import           Data.Pool                      ( createPool, Pool )
import Control.Monad.Trans.Reader (ReaderT, ask)
------------------------------------------------------------------

data DBConf = DBConf {hostUrl:: String, dbUser::String, dbPass::String, useReplica::Bool } 
      deriving(Show)

instance (Val a, Val b) => Val (Either a b) where
  val (Left  a) = val (True, a)
  val (Right a) = val (False, a)
  cast' (Database.MongoDB.Array (x : y : [])) =
    let (Just x_) = cast' x
    in  if x_
          then let (Just y_) = (cast' y) in Just (Left y_)
          else let (Just y_) = cast' y in Just (Right y_)

  cast' _ = Nothing

instance (Val a, Val b) => Val (a, b) where
  val (a, b) = val [val a, val b]
  cast' (Database.MongoDB.Array (x : y : [])) = (,) <$> cast' x <*> cast' y
  cast' _ = Nothing


createDBConnection connData = do
  let url = hostUrl connData
  let username    = T.pack (dbUser connData)
  let pass    = T.pack $ dbPass connData
  replicaSet <- openReplicaSetSRV' url
  pipe       <- primary replicaSet
  auth       <- access pipe master (T.pack "admin") $ auth username pass
  return (auth, pipe)



setupDB connData = do
  pool <- createPool (createDBConnection connData)
                     (\(_, pipe) -> close pipe)
                     1
                     300
                     5
  return pool 

run' :: Action (ReaderT (Bool, Pipe) IO) a -> ReaderT (Bool, Pipe) IO a
run' act = do
  (auth, pipe) <- ask
  if auth 
  then access pipe master (T.pack "db_name") act
  else error "no access granted."


run :: Action IO a -> IO a
run act = do
  let host_addr = "x"
  mUseReplica <- (lookupEnv "DB_USE_REPLICA")
  db_name     <- (getEnv "DB_NAME")
  let dbUser = T.pack "x" -- TODO sys envs
  let dbPass = T.pack "x"
  if mUseReplica == Nothing || mUseReplica == (Just "false")
    then do -- local testing
      pipe <- connect $ host host_addr
      access pipe master (T.pack db_name) act
    else do -- server with replica set
      replicaSet <- openReplicaSetSRV' host_addr -- TODO this takes ages, would be great to have this lookup once at startup then never
      pipe       <- primary replicaSet
      auth       <- access pipe master (T.pack "admin") $ auth dbUser dbPass
      if auth
        then access pipe master (T.pack db_name) act
        else error ("no access granted: ")
      --closeReplicaSet replicaSet



class (ToBSON a, FromBSON a) => MongoObject a where {-MUST DEFINE: insertId, collection (where the UID of the object is, whats the collection of the object-}
  insertId:: Show x => x -> a -> a

  collection:: a -> Collection

  serialize:: a -> [Field]
  serialize = toBSON

  deserialize:: Document -> Maybe a
  deserialize = fromBSON


  {- 
  Insert a new (!) Mongo Object into the database 
  This will rais an error in case a object with an existing id will be inserted
  -}
  insertObject:: a ->  ReaderT (Bool, Pipe) IO a
  insertObject object = do
    id <- run' $ insert (collection (undefined::a)) (serialize object)
    return (insertId id object)

  {-
  Updates a current record or inserts a new one, in case nothing is defined 
  
  Note: it is assumed, that the object in the database is already known (so this does not generate a id)
  -}
  updateObject:: a -> IO ()
  updateObject object = do
    run $ save (collection (undefined::a)) (serialize object)

  {-
  Find a certaint object by its _id 
  E.g:
  findById "5ed69725c3b5a031cb000000" :: IO (Maybe Card)
  <=>
  db.getCollection('cards').find({"_id": ObjectId("5ed69725c3b5a031cb000000")})
  
  Note: that in order to find the correct object, a typecast is necessary
  -}
  findById:: String -> IO (Maybe a)
  findById id = findObject [T.pack ("_id") =: ObjId (read id)]

  {- 
  Find a certaint object by a custom selector:
  E.g: 
  (findObject [T.pack ("color") =: Red])::IO (Maybe Card) 
  <=> 
  db.getCollection('cards').findOne({"color": "Red"}) 
  
  Note: that in order to find the correct object, a typecast is necessary
  -}
  findObject:: Database.MongoDB.Selector -> IO (Maybe a)
  findObject selector = do
    document <- run $ findOne $ select selector (collection (undefined::a))
    return (case document of
      Nothing -> Nothing
      (Just doc) -> deserialize doc)
  {-
  This method finds all objects matching the selector and can be presorted by the sorter (both BSON objects)
  E.g: 
  (findObject [T.pack ("color") =: Red] [])::IO ([Maybe Card]) 
  <=> 
  db.getCollection('cards').find({"color": "Red"})
  
  Your can presort elements by certain values, by defining a sort object
  findObjects [] ["number" := val 1])::IO ([Maybe Card]
  <=>
  db.getCollection('cards').find({}).sort({"number": 1})

  Note: 
    1) That in order to find the correct object, a typecast is necessary
    2) It is not possible to Project objects because we want to read the full datatype out of the database
  -}

  findObjects::Database.MongoDB.Selector -> Order -> IO [(Maybe a)]
  findObjects selector sorter = do
      docs_raw <- run $ find (select selector (collection (undefined::a))){sort = sorter}  >>= rest
      let docs = (map deserialize docs_raw):: [Maybe a]
      return docs



--instance Serialize Test
