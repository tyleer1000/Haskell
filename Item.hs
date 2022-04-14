module Item where

import qualified Data.Map as M

--type ItemName = String

--displayItemName :: ItemName -> IO ()
--http://dev.stephendiehl.com/hask/#records
--https://stackoverflow.com/questions/28381055/not-in-scope-data-constructor
data Item = Item {iname :: ItemName, weight :: Integer} deriving (Show, Eq) 

--mkUniverse :: [Item] -> Universe 