module Direction where

data Direction
    = N
    | S
    | E
    | W
    deriving (Eq)

instance Show Direction where --need to implement the member show for type direction 
    show :: Direction -> String
    show N = "north"
    show E = "east"
    show W = "west"    
    show S = "south"

