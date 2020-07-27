{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Utils
  ( ObjectKey(..)
  , MongoObject(..)
  , FromBSON(..)
  , ToBSON(..)
  , DBConf(..)
  , setupDB
  )
where
import           Web.Spock                      ( getSpockPool
                                                , getState
                                                )
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

import           Data.Pool                      ( withResource
                                                , createPool
                                                , Pool
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                )
import           Control.Monad.Trans
import           Responses                      ( AppStateM
                                                , DBConf(..)
                                                , AppConfig(..)
                                                )
------------------------------------------------------------------
import           Text.Read                      ( readMaybe )


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



setupDB :: DBConf -> IO (Pool Pipe)
setupDB connData = do
  pool <- createPool (createDBConnection connData) (\pipe -> close pipe) 1 300 5 -- TODO refactor thos hardocded numbers
  return pool
 where
  createDBConnection :: DBConf -> IO Pipe
  createDBConnection connData = do
    let url      = hostUrl connData
    let username = T.pack (dbUser connData)
    let pass     = T.pack $ dbPass connData
    if useReplica connData
      then do
        replicaSet <- openReplicaSetSRV' url
        pipe       <- primary replicaSet
        auth       <- access pipe master (T.pack "admin") $ auth username pass
        if auth then return pipe else error "unable to auth database"
      else do
        pipe <- connect $ host url
        if username == ""
          then return pipe
          else do
            auth <- access pipe master (T.pack "admin") $ auth username pass
            if auth
              then return pipe
              else do
                error "unable to auth database"


runDB
  :: (MonadTrans t, MonadIO (t (AppStateM sess)))
  => Action IO b
  -> t (AppStateM sess) b
runDB act = do
  dbConfig <-
    getState
      >>= (\appCfg -> do
            liftIO $ return $ dbConf appCfg
          )
  pool <- getSpockPool
  liftIO $ withResource
    pool
    (\pipe -> access pipe master (T.pack (dbName dbConfig)) act)



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
  insertObject :: (MonadTrans t, MonadIO (t (AppStateM sess)))
    => a
    -> t (AppStateM sess) a
  insertObject object = do
    id <- runDB $ insert (collection (undefined :: a)) (serialize object)
    return (insertId id object)

  {-
  Updates a current record or inserts a new one, in case nothing is defined 
  
  Note: it is assumed, that the object in the database is already known (so this does not generate a id)
  -}
  updateObject :: (MonadTrans t, MonadIO (t (AppStateM sess)))
    => a
    -> t (AppStateM sess) ()
  updateObject object = do
    runDB $ save (collection (undefined::a)) (serialize object)

  {-
  Find a certaint object by its _id 
  E.g:
  findById "5ed69725c3b5a031cb000000" :: IO (Maybe Card)
  <=>
  db.getCollection('cards').find({"_id": ObjectId("5ed69725c3b5a031cb000000")})
  
  Note: that in order to find the correct object, a typecast is necessary
  -}
  findById:: (MonadTrans t, MonadIO (t (AppStateM sess))) => String -> t (AppStateM sess) (Maybe a)
  findById id =    
    let mobjId = readMaybe id :: Maybe ObjectId
    in
      findObject [T.pack ("_id") =: val mobjId]

  {- 
  Find a certaint object by a custom selector:
  E.g: 
  (findObject [T.pack ("color") =: Red])::IO (Maybe Card) 
  <=> 
  db.getCollection('cards').findOne({"color": "Red"}) 
  
  Note: that in order to find the correct object, a typecast is necessary
  -}
  findObject:: (MonadTrans t, MonadIO (t (AppStateM sess)))
    =>Database.MongoDB.Selector
    -> t (AppStateM sess) (Maybe a)
  findObject selector = do
    document <- runDB $ findOne $ select selector (collection (undefined::a))
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

  findObjects::(MonadTrans t, MonadIO (t (AppStateM sess))) => Database.MongoDB.Selector -> Order -> t (AppStateM sess) [(Maybe a)]
  findObjects selector sorter = do
      docs_raw <- runDB $ find (select selector (collection (undefined::a))){sort = sorter}  >>= rest
      let docs = (map deserialize docs_raw):: [Maybe a]
      return docs



--instance Serialize Test
