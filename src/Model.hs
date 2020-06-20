{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Database.MongoDB
import           GHC.Generics
import qualified Data.Text                     as T
import           System.Environment



instance (Val a, Val b) => Val (Either a b) where
  val (Left  a) = val (True, a)
  val (Right a) = val (False, a)
  cast' (Array (x : y : [])) =
    let (Just x_) = cast' x
    in  if x_
          then let (Just y_) = (cast' y) in Just (Left y_)
          else let (Just y_) = cast' y in Just (Right y_)

  cast' _ = Nothing

instance (Val a, Val b) => Val (a, b) where
  val (a, b) = val [val a, val b]
  cast' (Array (x : y : [])) = (,) <$> cast' x <*> cast' y
  cast' _                    = Nothing


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

data User = User {uid:: Maybe String, name::String, gameId:: String, handCards::[Card], hints::[(Either Color Int, Int)]}
              deriving(Show, Generic, Eq)

instance ToJSON User

instance FromJSON User

instance Val User where
  val user = case uid user of
    Nothing ->
      (Doc
        [ "name" := val (name user)
        , "gameId" := val (gameId user)
        , "handCards" := val (handCards user)
        , "hints" := val (hints user)
        ]
      )
    (Just id) ->
      (Doc
        [ "_id" := val (ObjId (read id))
        , "name" := val (name user)
        , "gameId" := val (gameId user)
        , "handCards" := val (handCards user)
        , "hints" := val (hints user)
        ]
      )

  cast' (Doc bson) = do
    let (Just id) = ((bson !? "_id") :: Maybe ObjectId)
    let _id       = Just (show id)
    name      <- (bson !? "name")
    gameId    <- (bson !? "gameId")
    handCards <- (bson !? "handCards")
    hints     <- (bson !? "hints")
    User <$> (Just _id) <*> name <*> gameId <*> handCards <*> hints
  cast' _ = Nothing

instance MongoObject User where
  collection _ = "users"

  insertId id user = user { uid = Just (show id) }

data GameStatus = Status {cards::[Card], chips:: Int, lives::Int, stacks:: [(Color, Maybe Card)], player::[User]}
              deriving(Show, Eq, Generic)

instance ToJSON GameStatus

instance FromJSON GameStatus

data Game = Game {gid:: Int, userIds:: [Int], userWhiteList::[Int], public:: Bool, running:: Bool, status:: GameStatus}
              deriving(Show, Eq, Generic)

