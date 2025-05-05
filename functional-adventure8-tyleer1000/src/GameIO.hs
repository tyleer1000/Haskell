module GameIO where

-- :m + Control.Monad.State
--either import Control.Monad.State or use runGameIO as a proxy for evalStateT
import Control.Monad.State
    ( forM_,
      MonadState(get, put),
      MonadTrans(lift),
      gets,
      modify,
      evalStateT,
      StateT )
import System.Exit ( ExitCode(ExitSuccess), exitWith )
--import System.IO not used

import GameState
import Player
import Room
import Command
import Item

type GameIO = StateT GameState IO

runGameIO :: GameIO a -> GameState -> IO a
runGameIO = evalStateT

effectChange :: (GameState -> GameState) -> GameIO ()
-- effectChange :: (GameState -> GameState) -> GameIO (GameState -> GameState)
--effectChange f = modify f
effectChange = modify 

prompt :: GameIO ()
prompt = lift $ putStr "-> "

printMessage :: GameIO ()
printMessage = do
    oldState <- get

    --creating a new version of s with message set to nothing. Put then updates the state with the newly created version
    put $ let newState = oldState {message = Nothing}
        in newState

    case message oldState of
        Just m -> lift $ putStrLn m
        Nothing -> pure ()

printDescription :: GameIO ()
printDescription = do 
    s <- get
    let r = currentRoom s
    --first layer is state monad, second layer is IO mondad. Lift lifts one layer up
    lift $ putStrLn $ desc r

printObjects :: GameIO ()
printObjects = do
    s <- get 
    let r = currentRoom s
    let os = objects r 

    case os of 
        [] -> pure ()
        _ -> do
            lift $ putStrLn "You see the following objects:"
            --three alternatives: 
            --void throws away result of IO, goes from [()] to (). show converts itemName to string
            --void $ mapM (\itemName -> lift $ putStrLn $ show itemName) os
            mapM_ (\itemName -> lift $ putStrLn $ show itemName) os 
            --traverse_ is like voided traverse
            --traverse_ (\itemName -> lift $ putStrLn $ show itemName) os 


printExits :: GameIO ()
printExits = do
    s <- get
    let r = currentRoom s
    let ex = exits r

    case ex of
        [] -> pure ()
        _ -> do
            lift $ putStrLn "There are exits in the following directions:"
            --show only the direction, the first value of the pair of values in exits
            mapM_ (\(dir, _) -> lift $ putStrLn $ show dir) ex

myPrint :: String -> GameIO ()
myPrint s = lift $ putStrLn s

printInventory :: GameIO ()
printInventory = do
    p <- gets player
    let i = inventory p

    case i of 
        [] -> lift $ putStrLn "You aren't carrying anything"
        _ -> do
            lift $ putStrLn "You are carrying the following objects:"
            mapM_ ( myPrint . show ) i

-- effectChange :: (GameState -> GameState) -> GameIO ()

actionOverList :: (ItemName -> GameState -> GameState)
                -> [ItemName] 
                -> GameIO ()
actionOverList action items = forM_ items $ \ name -> do
    -- s <- get
    -- put $ action name s
    -- effecChange abstracts the above two lines
    effectChange $ action name
    printMessage
 
finishGame :: GameIO ()
finishGame = do
    myPrint "You successfully brought the jug into the yard.\nCongrats! You win!"
    lift $ exitWith ExitSuccess 

exit :: GameIO ()
exit = do
    myPrint "Goodbye!"
    lift $ exitWith $ ExitSuccess
    --lift $ exitWith $ ExitFailure 0

checkGameOver :: GameIO ()
checkGameOver = do 
    s <- get 

    if haveWonGame s then finishGame
    else pure () 

syntaxError :: GameIO ()
syntaxError = myPrint "I don't understand that"

opening :: GameIO ()
opening = myPrint "Welcome to Functional Adventure!"

-- data Command 
--     = Inventory
--     | Look
--     | Drop [ItemName]
--     | Take [ItemName]
--     | Move Direction
--     | Exit

performCommand :: Command -> GameIO ()
performCommand command = case command of
    Inventory -> printInventory
    Look -> printDescription >> printObjects >> printExits
    Drop is -> actionOverList dropItem is
    Take is -> actionOverList takeItem is
    Move dir -> ( effectChange $ move dir ) >> printMessage
    Exit -> exit

performConjunction :: Conjunction -> GameIO ()
performConjunction = mapM_ performCommand

parseConjunction :: String -> GameIO ()
parseConjunction command = case parseInput command of 
    Nothing -> myPrint "I don't understand that"
    Just conjunction -> performConjunction conjunction

repl :: GameIO ()
repl =  do
    prmpt <- lift getLine 
    parseConjunction prmpt
    checkGameOver
