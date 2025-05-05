module Command where

import Data.Char ()
import Data.List ()
import Direction
import Item
import Text.Parsec hiding (choice, parse, runParser, sepBy, sepBy1, (<|>))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (P.try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . fmap try

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser ""

data Command 
    = Inventory
    | Look
    | Drop [ItemName]
    | Take [ItemName]
    | Move Direction
    | Exit
    deriving (Eq, Show)

type Conjunction = [Command]

itemNameTuple :: Parser (ItemName, ItemName)
itemNameTuple = (,) <$> itemNameP <*> itemNameP
    
-- itemNameP' :: Parser ItemName
-- itemNameP' = 
--     (Pot <$ string "pot")
--         <|> (Jug <$ string "jug")
--         <|> (Sandbag <$ string "sandbag")

itemNameP :: Parser ItemName
itemNameP = 
    choice
        [ Pot <$ string "pot",
          Jug <$ string "jug",
          Sandbag <$ string "sandbag",
          Stove <$ string "stove",
          Couch <$ string "couch",
          Tarragon <$ string "tarragon",
          Beans <$ string "beans",
          Grill <$ string "grill",
          Bed <$ string "bed"
        ]

nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = do
    i <- itemNameP
    pure $ [i] 

nounPhrase :: Parser [ItemName]
nounPhrase = sepBy1 itemNameP ( string ", " )

inventoryP :: Parser Command
inventoryP = Inventory <$ string "inventory" 

-- *> tells nounPhrase not to try to interpret "take" or " " as an item
takeP :: Parser Command
takeP = Take <$> ( string "take" *> string " " *> nounPhrase )

exitP :: Parser Command
exitP = Exit <$ string "exit" <|> string "quit"

-- *> tells nounPhrase not to try to interpret "drop" or " " as an item
dropP :: Parser Command
dropP = Drop <$> ( string "drop" *> string " " *> nounPhrase )

-- fmap = <$>
-- a <$ b = fmap ( const a ) b

lookP :: Parser Command
lookP = Look <$ string "look"  

directionP :: Parser Direction
directionP = 
    --show converts N to "north"
    choice 
        [ N <$ string "north",
          E <$ string "east",
          W <$ string "west",
          S <$ string "south"
        ]

moveP :: Parser Command
moveP = Move <$> directionP 

commandP :: Parser Command
commandP = 
     choice 
        [ inventoryP,
          lookP,
          takeP,
          dropP,
          moveP,
          exitP
        ]

--will try and find as many commands as possible, separated by the keyword and
conjunctionP :: Parser Conjunction
conjunctionP = sepBy1 commandP ( string " and " ) <* P.eof

-- data Either a b = Left a | Right b
-- data Maybe a = Nothing | Just 1
-- data Int = 1 | 2 | 3 | ..

parseInput :: String -> Maybe Conjunction 
parseInput cmd = case parse conjunctionP cmd of
    Left _ -> Nothing
    Right listOfCommands -> Just listOfCommands

--applicative example:
-- let b = Just 1
-- let c = Just 2
-- (+) <$> b <*> c = Just 3