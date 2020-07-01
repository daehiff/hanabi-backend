{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model where

import           Data.Aeson                     ( ToJSON()
                                                , FromJSON(parseJSON)
                                                )
import           Data.Aeson.Types        hiding ( String )
import           Database.MongoDB
import           GHC.Generics
import qualified Data.Text                     as T
import           System.Environment             ( getEnv )



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


run act = do
  host_addr <- (getEnv "DB_ADDR")
  db_name   <- (getEnv "DB_NAME")
  pipe      <- connect $ host host_addr
  access pipe master (T.pack db_name) act

class (Val a) => MongoObject a where {-MUST DEFINE: insertId, collection (where the UID of the object is, whats the collection of the object-}
  insertId:: Show x => x -> a -> a

  collection:: a -> Collection

  serialize:: a -> [Field]
  serialize obj = a where Doc a = val obj

  deserialize:: Document -> Maybe a
  deserialize doc = cast' (Doc doc)


  {- 
  Insert a new (!) Mongo Object into the database 
  This will rais an error in case a object with an existing id will be inserted
  -}
  insertObject:: a -> IO a
  insertObject object = do
    id <- run $ insert (collection (undefined::a)) (serialize object)
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


data Color = Red | Blue | White | Yellow | Green | Rainbow
            deriving(Generic, Show, Eq )

map_color_string :: Color -> String
map_color_string Red     = "Red"
map_color_string Blue    = "Blue"
map_color_string White   = "White"
map_color_string Yellow  = "Yellow"
map_color_string Green   = "Green"
map_color_string Rainbow = "Rainbow"

map_string_color ("Red"    ) = Just Red
map_string_color ("Blue"   ) = Just Blue
map_string_color ("White"  ) = Just White
map_string_color ("Yellow" ) = Just Yellow
map_string_color ("Green"  ) = Just Green
map_string_color ("Rainbow") = Just Rainbow
map_string_color _           = Nothing

instance ToJSON Color

instance FromJSON Color

instance Val Color where
  val color = val (map_color_string color)
  cast' (String color) = map_string_color color
  cast' _              = Nothing

data Card = Card {cid::Maybe String, color::Color, number::Int}
              deriving(Generic, Show, Eq)

instance ToJSON Card

instance FromJSON Card

instance Val Card where
  val card = case cid card of
    (Just id) ->
      (Doc
        [ "_id" := val (ObjId (read id))
        , "color" := val (color card)
        , "number" := val (number card)
        ]
      )
    Nothing ->
      (Doc ["color" := val (color card), "number" := val (number card)])

  cast' (Doc bson) = do
    let (Just id) = ((bson !? "_id") :: Maybe ObjectId)
    let _id       = Just (show id)
    let color     = (bson !? "color")
    let number    = (bson !? "number")
    Card <$> (Just _id) <*> color <*> number
  cast' _ = Nothing

instance MongoObject Card where
  collection _ = "cards"

  insertId id card = card { cid = Just (show id) }



data User = User {uid::Maybe String,username::String, email::String, password_hash:: Maybe String ,sessions::[String]} deriving (Show, Generic, Eq)

instance ToJSON User where
  toJSON user = object
    [ "uid" .= uid user
    , "username" .= (username user)
    , "email" .= email user
    , "sessions" .= (toJSON (sessions user))
    ]


instance FromJSON User where
  parseJSON (Object v) = do
    uid      <- v .: "uid"
    username <- v .: "username"
    email    <- v .: "email"
    sessions <- v .: "sessions"
    return User { uid           = uid
                , username      = username
                , email         = email
                , password_hash = Nothing
                , sessions      = sessions
                }
instance Val User where
  val user = case uid user of
    (Just id) ->
      (Doc
        [ "_id" := val (ObjId (read id))
        , "username" := val (username user)
        , "password_h" := val (password_hash user)
        , "email" := val (email user)
        , "sessions" := val (sessions user)
        ]
      )
    Nothing ->
      (Doc
        [ "username" := val (username user)
        , "password_h" := val (password_hash user)
        , "email" := val (email user)
        , "sessions" := val (sessions user)
        ]
      )

  cast' (Doc bson) = do
    let (Just id) = ((bson !? "_id") :: Maybe ObjectId)
    let _id       = Just (show id)
    let username  = (bson !? "username")
    let password  = (bson !? "password_h")
    let email     = (bson !? "email")
    let sessions  = (bson !? "sessions")
    User <$> (Just _id) <*> username <*> email <*> password <*> sessions

instance MongoObject User where
  insertId id user = user { uid = Just (show id) }

  collection _ = "users"


data Session = Session {sid:: Maybe String, sessionUser::String} deriving (Show, Generic, Eq)
instance ToJSON Session
instance FromJSON Session

instance Val Session where
  val session = case sid session of
    Nothing    -> (Doc ["user" := val (sessionUser session)])
    (Just _id) -> (Doc ["_id" := val _id, "user" := val (sessionUser session)])

  cast' (Doc bson) = do
    let (Just id) = ((bson !? "_id") :: Maybe ObjectId)
    let _id       = Just (show id)
    user <- (bson !? "user")
    return Session { sid = _id, sessionUser = user }

instance MongoObject Session where
  collection _ = "sessions"

  insertId id session = session { sid = Just (show id) } 



-- > insertObject Session{sid=Nothing, sessionUser="sadsfg"}
