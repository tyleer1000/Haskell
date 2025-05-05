
module Room where

import Item
import Direction
import Data.List ( delete )


--type RoomName = String
data RoomName
  = Kitchen
  | Pantry
  | Yard
  | LivingRoom
  | Bedroom
  deriving (Eq, Ord)

instance Show RoomName where
    show :: RoomName -> String
    show Kitchen = "kitchen"
    show Pantry = "pantry"
    show Yard = "yard"
    show LivingRoom = "livingRoom"
    show Bedroom = "bedroom"

type Exit = (Direction, RoomName) --Exit is a pair

data Room = Room --data is defining a new type
    { rname :: RoomName
    , desc :: String
    , exits :: [Exit]
    , objects :: [ItemName]
    } deriving ( Show , Eq )

allRooms :: [Room]
allRooms = [bedroom, livingRoom, kitchen, pantry, yard]

--Ex: input: Room "den" "Fun place" [] ["fork" , "spoon"]
--output: Room {rname = "den", desc = "Fun place", exits = [], objects = ["fork","spoon"]}

bedroom :: Room 
bedroom = Room 
    { rname = Bedroom
    , desc = "The bedroom, where we sleep"
    , exits = [(S, LivingRoom)]
    , objects = [Bed]
    } 

livingRoom :: Room 
livingRoom = Room 
    { rname = LivingRoom
    , desc = "The livingRoom, where we hang out"
    , exits = [(S, Kitchen), (N, Bedroom)]
    , objects = [Couch, Jug, Sandbag]
    } 

kitchen :: Room 
kitchen = Room 
    { rname = Kitchen
    , desc = "The kitchen, where we cook"
    , exits = [(S, Yard), (E, Pantry), (N, LivingRoom)]
    , objects = [Pot, Stove]
    } 

yard :: Room 
yard = Room 
    { rname = Yard
    , desc = "The yard, where we grill"
    , exits = [(N, Kitchen)]
    , objects = [Grill]
    } 

pantry :: Room 
pantry = Room 
    { rname = Pantry
    , desc = "The pantry, where we store food"
    , exits = [(W, Kitchen)]
    , objects = [Tarragon, Beans]
    } 

roomName :: [RoomName]
roomName = roomNames

roomNames :: [RoomName]
roomNames = map rname allRooms 

addItem :: ItemName -> Room -> Room
addItem item room = room {objects = item : objects room}
-- another option
-- can use an as pattern and a match on the field
-- addItem item room@Room [objects] = room {objects = item : objects}

removeItem :: ItemName -> Room -> Room
removeItem item room@Room {objects} = room {objects = delete item objects }

hasObjects :: Room -> Bool
hasObjects = not . null . objects
-- or: hasObjects rm = not $ null (objects (rm))
