module Example where

import System.Random
import Control.Monad ( replicateM , forM )

import Item
import Direction
import Room
import Player 
import GameState

import Data.Map (elems) --elems :: Map k a -> [a]

choose :: [a] -> IO a
choose xs = do --must be do because it's an IO function 
    index <- randomRIO(0, length xs -1) --must import system.Random(randomRIO) to be able to use this. Returns integer between 0, xs. Now we have to 

    pure $ xs !! index 

rand1 :: IO Int
rand1 = randomRIO (1, 4)

rand2 :: IO Int
rand2 = randomRIO (6, 8)

--Search type in Hoogle: 
--https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Monad.html#v:replicateM
-- replicateM :: Applicative IO => Int -> IO a -> IO [a]
--https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Monad.html#v:forM
exampleList :: IO a -> IO Int -> IO [a]
exampleList randomA randomLength = do
    len <- randomLength
    -- replicateM len randomA -- like a for loop that runs len times on range defined by randomA. Each run ge
    forM [1..len] $ \ _ -> do -- $ \ i is anonymous function/lambda. $ is replacement for putting parenthesis around all that follows. All to the right is an argument 
        a <- randomA
        pure a
--exampleList rand1 rand2

--http://learnyouahaskell.com/types-and-typeclasses
--week 3 pdf Type Class
class Example a where
    example :: IO a 

-- data GameState = GameState
--  {
--     message :: Maybe String,
--     gmap :: GameMap,
--     universe :: Universe,
--     player :: Player
--  }


instance Example GameState where
    example :: IO GameState
    example = do
        messageInt <- randomRIO (1, 3)
        let newMessage = messageHelper messageInt
        roomInt <- randomRIO (2, 3)
        newRooms <- replicateM roomInt example
        itemInt <- randomRIO (5, 10)
        itemUniv <- replicateM itemInt example
        newPlayer <- example
        pure $ GameState
            {
                message = newMessage,
                gmap = mkMap newRooms,
                universe = mkUniverse itemUniv,
                player = newPlayer
            }
        where
            messageHelper :: Int -> Maybe String
            messageHelper 1 = Just "One possible message"
            messageHelper 2 = Just "Yet another possible message"
            messageHelper _ = Nothing
            

--want to get length of univ of items. Use: elems :: Map k a -> [a]
--https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Internal.html#v:elems
instance Example Item where
    example :: IO Item 
    example = do
        let ls = elems univ --stores our list of items 
        let len = length (ls) --returns 9, in this case. If we use <-, value to right must have type IO a 
        index <- randomRIO (0, len -1) --rand int between 1 and 9, in this case. index is the index of the list of items 
        randWeight <- randomRIO (0, 100) -- extracts random weight. We can't put randomRIO next to weight because weight needs type Integer, not IO Integer
        --need parenthesis around (ls !! index) because we are modifying the record (weight)
        pure $ (ls !! index) { weight = randWeight} -- like Choose in Lab 3. returns the item in univ at index 
--to run: example :: IO Item 

--implement like IO Item 
instance Example Direction where
    example :: IO Direction
    example = do 
       let ls = [N, S, W, E]
       index <- randomRIO (0, 3)
       pure $ ls !! index
--example :: IO Direction. Shows value of Direction, like "south"

exitExample :: IO Exit 
exitExample = do 
    direction <- example :: IO Direction
    let len = length (roomName) --gets length of roomName from Room module
    index <- randomRIO (0, len -1)
    pure $ (direction, roomName !! index) -- creates a pair with direction and random room 
--exitExample :: IO Exit

instance Example Room where
    example :: IO Room
    example = do 
        let len = length (roomName) 
        index <- randomRIO (0, len -1)
        room <- pure $ roomName !! index --stores randomly generated room
        let newDesc = "You are in the " ++ show room ++ " which is a randomly generated room"
        newExits <- exampleList (exitExample) (randomRIO(2, 4)) --arrow <- extracts a concrete value, rather an an IO value
        newObjects <- exampleList (example :: IO Item) (randomRIO(2, 5)) -- returns list of items, now we need a list of item names 
        pure $ Room 
                { rname = room
                , desc = newDesc
                , exits = newExits
                , objects = map iname newObjects --maps iname to newObjects to get list of item names 

                } 
-- example :: IO Room

--https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:max
--https://downloads.haskell.org/~ghc/6.12.1/docs/html/libraries/containers-0.3.0.0/Data-Map.html#v%3AfindMax
instance Example Player where
    example :: IO Player 
    example = do
        newInventory <- exampleList (example :: IO Item) (randomRIO(0, 10)) --generate random inventory between 0 and 10 items
       -- let newMaxWeight = findMax newInventory --need to get max weight of the inventory
        newLocation' <- example :: IO Room 
        pure $ Player
            { inventory = map iname newInventory --maps names of the inventory
            , maxWeight = 16 --lightest item: beans 1, + heaviest item: bed 15
            , location = rname newLocation' --don't use map, because there's only one location
            }

--example :: IO Player
    -- player type: 
    --{ inventory = []
    --, maxWeight = 100
    --, location = "kitchen" 