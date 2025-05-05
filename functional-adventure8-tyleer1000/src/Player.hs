module Player where

import Item
import Room
import Data.List (delete)

data Player = Player -- if new module isn't available at first, try :l Player. Then :k Player to confirm Player is an available type
    { inventory :: [ItemName]
    , maxWeight :: Integer
    , location :: RoomName
    } deriving ( Show , Eq )
-- :t Player is: Player :: [ItemName] -> Integer -> RoomName -> Player
-- we get the value of type player by inputting these three arguments 

--input: Player ["glove", "hat"] 75 "roofdecK" or let tempPlayer = Player ["glove", "hat"] 75 "roofdecK". This stores tempPlayer for later use
--output: Player {inventory = ["glove","hat"], maxWeight = 75, location = "roofdecK"}

--tempPlayer = Player ["glove", "hat"] 75 "roofdecK"

addItem :: ItemName -> Player -> Player
addItem item player = player { inventory = item : inventory player }
-- additem "shoe" tempPlayer

--https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-List.html#v:delete
removeItem :: ItemName -> Player -> Player 
removeItem item player = player { inventory = delete item (inventory player) }
--removeItem "hat" tempPlayer

newLocation :: RoomName -> Player -> Player 
newLocation loc player = player { location = loc } --update location value to loc

--https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-List.html#v:null
--https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:not
isCarryingAnything :: Player -> Bool 
isCarryingAnything player = not (null (inventory player)) --null returns true if inventory is empty, so not reverses it 


you :: Player
you = Player 
    { inventory = []
    , maxWeight = 100
    , location = Kitchen
    } 
