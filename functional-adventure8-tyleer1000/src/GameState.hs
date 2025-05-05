module GameState where

import Data.List ( find )
import Control.Exception
import qualified Data.Map as M

import Item
import Room as Room
import Player as Player
import Direction
--import GHC.Base (undefined)
--import GHC.ExecutionStack (getStackTrace)
import Control.Monad ( void )

-- This is a type alias, simple way to refer to this map of drawn out name of room names to room
type GameMap = M.Map RoomName Room

gameMap :: GameMap
gameMap = mkMap allRooms

initialState :: GameState 
initialState = GameState 
    {
        message = Nothing,
        gmap = gameMap,
        universe = univ,
        player = you
    }

-- record type of GameState
data GameState = GameState
 {
    message :: Maybe String,
    gmap :: GameMap,
    universe :: Universe,
    player :: Player
 } deriving ( Show )

mkMap :: [Room] -> GameMap
mkMap rooms = M.fromList myroomlist
    where
        myroomlist :: [ ( RoomName , Room ) ]
        myroomlist = fmap helper rooms -- rooms is of type list, so it's the f a. f b is list of (rname room, room)

        helper :: Room -> ( RoomName , Room )
        helper room = ( rname room , room )

data KeyError = KeyError
  deriving Show

instance Exception KeyError

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
    = case M.lookup rname mp of
        Just room -> room
        Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

--https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Internal.html#v:insert
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap = M.insert
-- rname is k, room is v, GameMap is Map k v. setRoomMap is a version of M.insert that only works with the given types

setMessage :: String -> GameState -> GameState
setMessage newMessage gst = gst { message = Just newMessage } --message type is Maybe String, so we put Just newMessage

currentInventory :: GameState -> [ItemName]
currentInventory gst = inventory ( player gst )
--currentInventory initialState -> []

--https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html#v:find
currentRoom :: GameState -> Room
currentRoom gst = curRoom
    where
        curRoomName = location ( player gst )

        curRoom :: Room
        curRoom = case ( find roomComparer $ M.elems $ gmap gst ) of
            Nothing -> throw KeyError
            Just r  -> r

        roomComparer :: Room -> Bool
        roomComparer room = rname room == curRoomName
--currentRoom initialState

nearbyObjects :: GameState -> [ItemName]
nearbyObjects gst = objects (currentRoom gst)
--nearbyObjects initialState

--https://wiki.haskell.org/Handling_errors_in_Haskell
-- type Error a = Either String a
-- 
-- data (Either error) a = Left error | Right a
-- data (Maybe) a = Nothing | Just a
-- 
-- Error GameState

-- weightCheck :: ItemName -> GameState -> Error GameState
-- anywhereDropCheck :: ItemName -> GameState -> Error GameState
-- inRoomDropCheck :: ItemName -> GameState -> Error GameState


takeItem :: ItemName -> GameState -> GameState
takeItem item gst = case checkedState of
    Left errorMessage -> gst { message = Just errorMessage }
    Right validGst -> validGst
        { gmap = setRoomMap (rname cR) (Room.removeItem item cR) ( gmap gst )
        , player = Player.addItem item ( player gst )
        , message = Just ("You take the " ++ show item ++ ".")
        } 
    where 
        cR = currentRoom gst

        checkedState :: Error GameState
        checkedState = do
            void $ alreadyHaveTakeCheck item gst --we only want the possible error, not the result. https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Monad.html#v:void
            void $ inRoomTakeCheck item gst
            weightCheck item gst
 
dropItem :: ItemName -> GameState -> GameState
dropItem item gst = case checkedState of
    Left errorMessage -> gst { message = Just errorMessage }
    Right validGst -> validGst
        { gmap = setRoomMap (rname cR) (Room.addItem item cR) ( gmap gst )
        , player = Player.removeItem item ( player gst )
        , message = Just $ "You drop the " ++ show item ++ "."
        }
    where 
        cR = currentRoom gst

        checkedState :: Error GameState
        checkedState = do
          _ <- anywhereDropCheck item gst
          _ <- inRoomDropCheck item gst 
          pure gst

--http://zvon.org/other/haskell/Outputprelude/elem_f.html
inventoryWeight :: GameState -> Integer
inventoryWeight gst = sum $ fmap weight $ filter ( ( `elem` ( inventory $ player gst ) ) . iname ) $ M.elems univ
--     where 
--         inv = inventory ( player gst ) -- now we have list of item names 
--         invWeight = sum $ fmap weight $ filter ( `elem` ( inventory ( player gst ) ) . iname ) $ M.elems univ --map weight to elements of inv in univ. then sum

-- univ :: Universe
-- univ = mkUniverse [pot, stove, couch, sandbag, jug, grill, bed, tarragon, beans]

-- record type of GameState
-- data GameState = GameState
--  {
--     message :: Maybe String,
--     gmap :: GameMap,
--     universe :: Universe,
--     player :: Player
--  } deriving ( Show )

type Error a = Either String a

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck itm gst =
    if hasItem
        then Left ( "You are already carrying the " ++ show itm )
        else Right gst 
    where 
        inv = inventory ( player gst )
        hasItem = elem itm inv

inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck itm gst = 
    --use case, as isITem is type Maybe Bool http://learnyouahaskell.com/syntax-in-functions
    case newIsItem of 
        Nothing -> Left "Room doesn't exist"
        Just isItem' -> 
            if isItem'
                then Right gst
                else Left ( "There is no " ++ show itm ++ " in this room")
    where 
        plyrRoomNm :: RoomName
        plyrRoomNm = location ( player gst ) --get name of player's room

        newIsItem :: Maybe Bool
        newIsItem = do
            room <- M.lookup plyrRoomNm ( gmap gst )
            pure $ elem itm ( objects room )

        -- mroom :: Maybe Room
        -- mroom = M.lookup plyrRoomNm ( gmap gst ) -- get Maybe room

        -- isItem :: Maybe Bool
        -- isItem = fmap (elem itm) (fmap objects mroom) --mroom is maybe room, we use fmap to get Maybe [ItemName]. 

-- type GameMap = M.Map RoomName Room

-- data Room = Room --data is defining a new type
--     { rname :: RoomName
--     , desc :: String
--     , exits :: [Exit]
--     , objects :: [ItemName]
--     } deriving ( Show , Eq )

-- M.lookup :: Ord k => k -> Map k a -> Maybe a
--collaborated with June Cong on Functional Adventure 5
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck itm gst = case mTotal of 
    Nothing -> Left "Item does not exist"
    Just total -> 
        if total > maxWeight ( player gst )
            then Left "That's too much weight for you to carry."
            else Right gst
    where
        plyrItemNms :: [ ItemName ]
        plyrItemNms = inventory ( player gst )

        inventoryTotal :: Integer
        inventoryTotal = sum $ fmap weight $ filter ( ( `elem` plyrItemNms ) . iname ) $ M.elems $ universe gst 

        mItmWeight :: Maybe Integer
        mItmWeight = do
            item <- M.lookup itm (universe gst) 
            Just $ weight item 

        mTotal :: Maybe Integer
        mTotal = fmap (+ inventoryTotal) mItmWeight --fmap allows addition of inventoryTotal within maybe structure 

anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck itm gst = 
    if itm `elem` (plyrItemNms ++ roomItemNms)
        then Right gst
        else Left ("what do you mean, drop the " ++ show itm ++ "?")
    where
        plyrItemNms :: [ ItemName ]
        plyrItemNms = inventory ( player gst )

        roomItemNms :: [ ItemName ]
        roomItemNms = objects (currentRoom gst)

inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck itm gst =
    if itm `elem` roomItemNms
        then Left ("You aren't carrying the " ++ show itm ++ ".")
        else Right gst

    where 
        roomItemNms :: [ ItemName ]
        roomItemNms = objects (currentRoom gst)

roomHasObjects :: GameState -> Bool
roomHasObjects = hasObjects . currentRoom

-- hasObjects :: Room -> Bool
-- hasObjects = not . null . objects

destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir rm = snd <$> chosenExit
    where
        chosenExit :: Maybe Exit
        chosenExit = find ( (==dir) . fst ) $ exits rm

move :: Direction -> GameState -> GameState
move dir gst =
    case mloc of
        Nothing -> gst
            { message = Just "There's no exit in that direction"
            }
        Just loc -> gst
            { message = Just ("You go " ++ show(dir) ++ "." )
            , player = ( player gst ) { location = loc }
            }
    where
        room :: Room
        room = currentRoom gst

        mloc :: Maybe RoomName
        mloc = destinationName dir room

haveWonGame :: GameState -> Bool
haveWonGame gst = ( curLoc == Yard ) && ( Jug `elem` curInv )
    where
        curLoc = location $ player gst
        curInv = inventory $ player gst

