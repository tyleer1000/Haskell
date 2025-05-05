module Item where

import qualified Data.Map as M

--type ItemName = String (not used in FA6)
data ItemName
  = Pot
  | Jug
  | Sandbag
  | Stove
  | Couch
  | Tarragon
  | Beans
  | Grill
  | Bed
  deriving (Eq, Ord)

instance Show ItemName where
    show :: ItemName -> String
    show Pot = "pot"
    show Jug = "jug"
    show Sandbag = "sandbag"
    show Stove = "stove"
    show Couch = "couch"
    show Tarragon = "tarragon"
    show Beans = "beans"
    show Grill = "grill"
    show Bed = "bed"

displayItemName :: ItemName -> IO ()
displayItemName = undefined 


--https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Lazy.html#g:1
--https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Lazy.html#g:6
--http://dev.stephendiehl.com/hask/#records
--https://stackoverflow.com/questions/28381055/not-in-scope-data-constructor
--Recommended workflow: Check functions in Hoogle (can write the type arguments in Hoogle) check the types, use types as a hint to construct the function
data Item = Item
    { iname :: ItemName
    , weight :: Integer
    } deriving ( Show , Eq ) 

--exItem :: Item
--exItem = Item {iname = "fork", weight = 9}

type Universe = M.Map ItemName Item --M.Map is a type constructor that needs two arguments

--empty :: Map k a
--insert :: Ord k => k -> a -> Map k a -> Map k a

exPair :: ( Int , Bool )
exPair = ( 1 , True )

--exUni :: Universe --example of universe created with one item, exItem
--exUni = M.insert (iname exItem) exItem M.empty 

--https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Internal.html#v:fromList
mkUniverse :: [Item] -> Universe
mkUniverse is = M.fromList (map f is) --fromList needs a list of keys and values, map takes a function and a list
    where
        f :: Item -> ( ItemName , Item )
        f i = ( iname i , i )
-- is is the list of items. f converts an item into a key and value. 

pot :: Item
pot = Item
    { iname = Pot
    , weight = 2
    }  

jug :: Item 
jug = Item
    { iname = Jug
    , weight = 3
    }  

couch :: Item 
couch = Item
    { iname = Couch
    , weight = 10
    }  

stove :: Item 
stove = Item
    { iname = Stove
    , weight = 8
    }  

grill :: Item 
grill = Item
    { iname = Grill
    , weight = 11
    }  

bed :: Item 
bed = Item
    { iname = Bed
    , weight = 15
    }
    
sandbag :: Item 
sandbag = Item
    { iname = Sandbag
    , weight = 3
    }  

tarragon :: Item 
tarragon = Item
    { iname = Tarragon
    , weight = 1
    }  

beans :: Item 
beans = Item
    { iname = Beans
    , weight = 1
    }  

univ :: Universe
univ = mkUniverse [pot, stove, couch, sandbag, jug, grill, bed, tarragon, beans]

--search in Hoogle: Map k v -> [k]. We want a list of keys 
--https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Internal.html#v:keys
--keys :: Map k a -> [k]

itemNames :: [ItemName]
itemNames = M.keys univ --univ is a map of string (item name) and item. keys outputs the key of each item 



