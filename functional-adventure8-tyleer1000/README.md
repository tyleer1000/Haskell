## 1.3 Project, Part I

Let's devote the rest of the lab to the final project. You can just copy your
functional-adventure project from last week's homework into this directory.

## 1.4 The GameIO Module

Create a new module called `src/GameIO.hs`. Then, add the following module
declaration and module imports to the top of the file:

```haskell
module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item
```

To be able to `import Control.Monad.State`, you will also need to add `mtl` as a
dependency in `package.yaml` in the `dependencies:` section.

This module will be making use of lots of code from different parts of your
project. Thus, all the imports. `System.Exit` you're familiar with from `eliza`,
and `System.IO` is just needed for one little thing; we'll point that out when
we get to it.

### 1.4.1 Exercise: GameIO datatype

In `src/GameIO.hs`, define a type alias called `GameIO` that passes `IO` into
`StateT`, using `GameState` as a type for simulated state.

### 1.4.2 Exercise: effectChange

This isn't absolutely necessary to get your code to work — I'll just ask you to
write it in the interest of improving readability. In `src/GameIO.hs`, define a
value called `effectChange` with the following type signature:

```haskell
effectChange :: (GameState -> GameState) -> GameIO ()
```

`effectChange` takes a transition between game states as an input, and outputs a
value of type `GameIO` that performs that transition within the monadic `GameIO`
value. Hint: this is really, really simple—you can just use one of the standard
functions that's part of the state monad interface.

When you're done, you can test it out in the REPL like this:

```haskell
λ> gameplay = effectChange (setMessage "test message")
λ> execStateT gameplay initialState
GameState {message = Just "test message", gmap = fromList [(kitchen,Room {rname = kitchen, desc = "You are in a small kitchen.", exits = [(north,living room),(east,pantry),(south,yard)], objects = [pot,stove]}),(pantry,Room {rname = pantry, desc = "This is the pantry.", exits = [(west,kitchen)], objects = [tarragon,beans]}),(yard,Room {rname = yard, desc = "This is the yard.", exits = [(north,kitchen)], objects = [grill]}),(living room,Room {rname = living room, desc = "You are in a large living room.", exits = [(south,kitchen),(north,bedroom)], objects = [couch,jug,sandbag]}),(bedroom,Room {rname = bedroom, desc = "This is the master bedroom.", exits = [(south,living room)], objects = [bed]})], universe = fromList [(pot,Item {iname = pot, weight = 5}),(jug,Item {iname = jug, weight = 40}),(sandbag,Item {iname = sandbag, weight = 95}),(stove,Item {iname = stove, weight = 100}),(couch,Item {iname = couch, weight = 100}),(tarragon,Item {iname = tarragon, weight = 1}),(beans,Item {iname = beans, weight = 10}),(grill,Item {iname = grill, weight = 150}),(bed,Item {iname = bed, weight = 100})], player = Player {inventory = [], maxWeight = 100, location = kitchen}}
```

### 1.4.3 Exercise: prompt

In `src/GameIO.hs`, define a value called prompt with the following type:

```haskell
prompt :: GameIO ()
```

`prompt` prints the string "-> " (without a newline character) to the console,
which functions for the user as a visible prompt:

```haskell
λ> evalStateT prompt initialState
-> λ>
```

### 1.4.4 Exercise: printMessage

In `src/GameIO.hs`, define a value called `printMessage` with the following type
signature:

```haskell
printMessage :: GameIO ()
```

`printMessage` checks the current state of the game, and if there is `Just`
value in the message field of the current game state, it prints the message to
the screen, then sets the game state message field to `Nothing`. If there is a
`Nothing` value in the message field of the current game state, it does nothing.

When you're done, you can test `printMessage` out in the REPL like this:

```haskell
λ> gameplay = effectChange (setMessage "test message")
λ> execStateT gameplay initialState
GameState {message = Just "test message", gmap = fromList [(kitchen,Room {rname = kitchen, desc = "You are in a small kitchen.", exits = [(north,living room),(east,pantry),(south,yard)], objects = [pot,stove]}),(pantry,Room {rname = pantry, desc = "This is the pantry.", exits = [(west,kitchen)], objects = [tarragon,beans]}),(yard,Room {rname = yard, desc = "This is the yard.", exits = [(north,kitchen)], objects = [grill]}),(living room,Room {rname = living room, desc = "You are in a large living room.", exits = [(south,kitchen),(north,bedroom)], objects = [couch,jug,sandbag]}),(bedroom,Room {rname = bedroom, desc = "This is the master bedroom.", exits = [(south,living room)], objects = [bed]})], universe = fromList [(pot,Item {iname = pot, weight = 5}),(jug,Item {iname = jug, weight = 40}),(sandbag,Item {iname = sandbag, weight = 95}),(stove,Item {iname = stove, weight = 100}),(couch,Item {iname = couch, weight = 100}),(tarragon,Item {iname = tarragon, weight = 1}),(beans,Item {iname = beans, weight = 10}),(grill,Item {iname = grill, weight = 150}),(bed,Item {iname = bed, weight = 100})], player = Player {inventory = [], maxWeight = 100, location = kitchen}}
λ> evalStateT printMessage initialState
λ> gameplay2 = effectChange (setMessage "test message") >> printMessage
λ> evalStateT gameplay2 initialState
test message
```

### 1.4.5 Exercise: printDescription

In `src/GameIO.hs`, define a value called `printDescription` with the following
type signature:

```haskell
printDescription :: GameIO ()
```

`printDescription` prints a description of the room where the player is in the
current game state:

```haskell
λ> evalStateT printDescription initialState
You are in a small kitchen.
λ> newPlayer = (player initialState) { location = Pantry }
λ> newState = initialState { player = newPlayer }
λ> evalStateT printDescription newState
This is the pantry.
```

### 1.4.6 Exercise: printObjects

In `src/GameIO.hs`, define a value called `printObjects` with the following
type:

```haskell
printObjects :: GameIO ()
```

`printObjects` prints `"You see the following objects:"` followed by a list of
all the items in the room where the player is—assuming there are any. If there
are no objects in the room where the player is, it does nothing:

```haskell
λ> evalStateT printObjects initialState
You see the following objects:
pot
stove
λ> newState = takeItem Stove . takeItem Pot $ initialState
λ> evalStateT printObjects newState
```

(That may come out slightly different, depending on how you set the weights of
the pot and the stove, but the point is that `printObjects` should print nothing
if there are no objects left in the room).

# 2 Project

For the 'project' portion of the lab, we'll continue building out the `GameIO`
module with the almost the entire rest of the imperative code the game is going
to need to execute. There will just be one or two quick things to finish up on
the imperative front next week, and then you'll be able to play the game you
made!

### 2.0.1 Exercise: printExits

In `src/GameIO.hs`, define a value called `printExits` with the following type:

```haskell
printExits :: GameIO ()
```

`printExits` prints `"There are exits in the following directions:"` followed by
a list of all the directions there are exits in, in the room where the player
currently is. If there are no exits in the room where the player currently is,
it does nothing:

```haskell
λ> evalStateT printExits initialState
There are exits in the following directions:
north
east
south
λ> newPlayer = (player initialState) { location = Bedroom }
λ> newState = initialState { player = newPlayer }
λ> evalStateT printExits newState
There are exits in the following directions:
south
```

### 2.0.2 Exercise: printInventory

In `src/GameIO.hs`, define a value called `printInventory` with the following
type:

```haskell
printInventory :: GameIO ()
```

As the name suggests, `printInventory` checks the player's current inventory,
and if it's empty, prints `"You aren't carrying anything."` If the player's
current inventory is nonempty, then it prints `"You are carrying the following
items:"` followed by a list of all the `ItemName`-s in the player's inventory:

```haskell
λ> evalStateT printInventory initialState
You aren't carrying anything.
λ> newState = takeItem Pot . takeItem Stove $ initialState
λ> evalStateT printInventory newState
You are carrying the following objects:
pot
stove
```

### 2.0.3 Exercise: actionOverList

In `src/GameIO.hs`, define a value called `actionOverList` with the following
type:

```haskell
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
```

`actionOverList` takes a function describing an action on items (i.e. like
taking or dropping them) and a list of item names as input, and performs the
action on each item in the list, in order. Each time it executes an action, it
runs `printMessage`.

When you're done, you should be able to test `actionOverList` in the REPL like so:

```haskell
λ> gameplay = actionOverList takeItem [Pot, Stove]
λ> evalStateT gameplay initialState
You take the pot.
You take the stove.
λ> evalStateT (gameplay >> actionOverList dropItem [Pot, Stove]) initialState
You take the pot.
You take the stove.
You drop the pot.
You drop the stove.
```

### 2.0.4 Exercise: finishGame

In `src/GameIO.hs`, define a value called `finishGame` with the following type:

```haskell
finishGame :: GameIO ()
```

`finishGame` performs the action of printing a success message to the screen,
then quits the program. This is what you will run when the user wins the game.
You can make the message anything you like, but it should print the message and
then exit with a zero exit status.

```haskell
λ> evalStateT finishGame initialState
You successfully brought the jug into the yard.
Congrats! You win!
*** Exception: ExitSuccess
```

### 2.0.5 Exercise: exit

In `src/GameIO.hs`, define a value called exit with the following type:

```haskell
exit :: GameIO ()
```

`exit` is for when the user decides to quit the game, rather than when they win.
`exit` prints the message `"Goodbye!"`, then exits the game with a zero exit
status:

```haskell
λ> evalStateT exit initialState
Goodbye!
*** Exception: ExitSuccess
```

### 2.0.6 Exercise: checkGameOver

In `src/GameIO.hs`, define a value called `checkGameOver` with the following
type:

```haskell
checkGameOver :: GameIO ()
```

`checkGameOver` checks whether the current game state is the winning state.
(Recall that we defined what it means to win the game in the `GameState` module
with the `haveWonGame` predicate.) If yes, it runs `finishGame`. Otherwise, it
does nothing:

```haskell
λ> evalStateT checkGameOver initialState
λ> newPlayer = (player initialState) { location = Yard, inventory = [Jug] }
λ> newState = initialState { player = newPlayer }
λ> evalStateT checkGameOver newState
You successfully brought the jug into the yard.
Congrats! You win!
*** Exception: ExitSuccess
```

### 2.0.7 Exercise: syntaxError

In `src/GameIO.hs`, define a value called `syntaxError` with the following type:

```haskell
syntaxError :: GameIO ()
```

`syntaxError` prints the message `'I don't understand that'`:

```haskell
λ> evalStateT syntaxError initialState
I don't understand that.
```

### 2.0.8 Exercise: opening

In `src/GameIO.hs`, define a value called opening with the following type:

```haskell
opening :: GameIO ()
```

`opening` prints the message `"Welcome to Functional Adventure!"`

```haskell
λ> evalStateT opening initialState
Welcome to Functional Adventure!
```

### 2.0.9 Exercise: performCommand

In `src/GameIO.hs`, define a value called `performCommand` with the following
type:

```haskell
performCommand :: Command -> GameIO ()
```

`performCommand` takes any `Command` as an input, and executes the action
corresponding to the command. Note that:

1. `performCommand` should make the `Look` command run `printDescription`, then run `printObjects` then run `printExits`
1. `performCommand` should make the `Move` command move the player in the relevant direction, then run `printMessage`
1. `performCommand` should make `Inventory` run `printInventory`
1. `performCommand` should make a `Take` command perform the `takeItem` action over the list of item names
1. `performCommand` should make a `Drop` command perform the `dropItem` action over the list of item names
1. `performCommand` should make `Exit` perform an `exit`
1. `performCommand` should make `Inventory` perform `printInventory`

When you're done, you should be able to test it out in the REPL like so:

```haskell
λ> evalStateT (performCommand Look) initialState
You are in a small kitchen.
You see the following objects:
pot
stove
There are exits in the following directions:
north
east
south
λ> evalStateT (performCommand $ Move N) initialState
You go north.
λ> execStateT (performCommand $ Move N) initialState
You go north.
GameState {message = Nothing, gmap = fromList [(kitchen,Room {rname = kitchen, desc = "You are in a small kitchen.", exits = [(north,living room),(east,pantry),(south,yard)], objects = [pot,stove]}),(pantry,Room {rname = pantry, desc = "This is the pantry.", exits = [(west,kitchen)], objects = [tarragon,beans]}),(yard,Room {rname = yard, desc = "This is the yard.", exits = [(north,kitchen)], objects = [grill]}),(living room,Room {rname = living room, desc = "You are in a large living room.", exits = [(south,kitchen),(north,bedroom)], objects = [couch,jug,sandbag]}),(bedroom,Room {rname = bedroom, desc = "This is the master bedroom.", exits = [(south,living room)], objects = [bed]})], universe = fromList [(pot,Item {iname = pot, weight = 5}),(jug,Item {iname = jug, weight = 40}),(sandbag,Item {iname = sandbag, weight = 95}),(stove,Item {iname = stove, weight = 95}),(couch,Item {iname = couch, weight = 100}),(tarragon,Item {iname = tarragon, weight = 1}),(beans,Item {iname = beans, weight = 10}),(grill,Item {iname = grill, weight = 150}),(bed,Item {iname = bed, weight = 100})], player = Player {inventory = [], maxWeight = 100, location = living room}}
λ> evalStateT (performCommand $ Take [Pot, Stove]) initialState
You take the pot.
You take the stove.
λ> execStateT (performCommand $ Take [Pot, Stove]) initialState
You take the pot.
You take the stove.
GameState {message = Nothing, gmap = fromList [(kitchen,Room {rname = kitchen, desc = "You are in a small kitchen.", exits = [(north,living room),(east,pantry),(south,yard)], objects = []}),(pantry,Room {rname = pantry, desc = "This is the pantry.", exits = [(west,kitchen)], objects = [tarragon,beans]}),(yard,Room {rname = yard, desc = "This is the yard.", exits = [(north,kitchen)], objects = [grill]}),(living room,Room {rname = living room, desc = "You are in a large living room.", exits = [(south,kitchen),(north,bedroom)], objects = [couch,jug,sandbag]}),(bedroom,Room {rname = bedroom, desc = "This is the master bedroom.", exits = [(south,living room)], objects = [bed]})], universe = fromList [(pot,Item {iname = pot, weight = 5}),(jug,Item {iname = jug, weight = 40}),(sandbag,Item {iname = sandbag, weight = 95}),(stove,Item {iname = stove, weight = 95}),(couch,Item {iname = couch, weight = 100}),(tarragon,Item {iname = tarragon, weight = 1}),(beans,Item {iname = beans, weight = 10}),(grill,Item {iname = grill, weight = 150}),(bed,Item {iname = bed, weight = 100})], player = Player {inventory = [stove,pot], maxWeight = 100, location = kitchen}}
λ> dropState = takeItem Pot . takeItem Stove $ initialState
λ> evalStateT (performCommand $ Drop [Pot, Stove]) dropState
You drop the pot.
You drop the stove.
λ> execStateT (performCommand $ Drop [Pot, Stove]) dropState
You drop the pot.
You drop the stove.
GameState {message = Nothing, gmap = fromList [(kitchen,Room {rname = kitchen, desc = "You are in a small kitchen.", exits = [(north,living room),(east,pantry),(south,yard)], objects = [stove,pot]}),(pantry,Room {rname = pantry, desc = "This is the pantry.", exits = [(west,kitchen)], objects = [tarragon,beans]}),(yard,Room {rname = yard, desc = "This is the yard.", exits = [(north,kitchen)], objects = [grill]}),(living room,Room {rname = living room, desc = "You are in a large living room.", exits = [(south,kitchen),(north,bedroom)], objects = [couch,jug,sandbag]}),(bedroom,Room {rname = bedroom, desc = "This is the master bedroom.", exits = [(south,living room)], objects = [bed]})], universe = fromList [(pot,Item {iname = pot, weight = 5}),(jug,Item {iname = jug, weight = 40}),(sandbag,Item {iname = sandbag, weight = 95}),(stove,Item {iname = stove, weight = 95}),(couch,Item {iname = couch, weight = 100}),(tarragon,Item {iname = tarragon, weight = 1}),(beans,Item {iname = beans, weight = 10}),(grill,Item {iname = grill, weight = 150}),(bed,Item {iname = bed, weight = 100})], player = Player {inventory = [], maxWeight = 100, location = kitchen}}
λ> evalStateT (performCommand Exit) initialState
Goodbye!
*** Exception: ExitSuccess
λ> evalStateT (performCommand Inventory) initialState
You aren't carrying anything.
λ> evalStateT (performCommand Inventory) dropState
You are carrying the following objects:
pot
stove
```

### 2.0.10 Exercise: performConjunction

In `src/GameState.hs`, define a value called `performConjunction` with the following type:

```haskell
performConjunction :: Conjunction -> GameIO ()
```

`performConjunction` performs every command in a `Conjunction`, in order:

```haskell
λ> evalStateT (performConjunction [Inventory, Look]) initialState
You aren't carrying anything.
You are in a small kitchen.
You see the following objects:
pot
stove
There are exits in the following directions:
north
east
south
λ> evalStateT (performConjunction [Move N, Move N, Move N]) initialState
You go north.
You go north.
There is no exit in that direction.
λ> evalStateT (performConjunction [Move N, Move N, Take [Bed]]) initialState
You go north.
You go north.
You take the bed.
```

### 2.0.11 Exercise: parseConjunction

In `src/GameIO.hs`, define a value called `parseConjunction` with the following
type:

```haskell
parseConjunction :: String -> GameIO ()
```

`parseConjunction` parses an input string, and if the parse succeeds, runs
`performConjunction` on the result. If the parse fails, it runs `syntaxError`:

```haskell
λ> evalStateT (parseConjunction "take pot and inventory") initialState
You take the pot.
You are carrying the following objects:
pot
λ> evalStateT (parseConjunction "owahfhewoiejr") initialState
I don't understand that.
```

### 2.0.12 Exercise: repl

In `src/GameIO.hs`, define a value called repl with the following type:

```haskell
repl :: GameIO ()
```

`repl` performs one round of printing the prompt, getting input from the user,
parsing it, performing the command the input denotes if the parse succeeds and
printing a syntax error message otherwise, then running `checkGameOver`:

```haskell
λ> evalStateT repl initialState
-> inventory
You aren't carrying anything.
λ> evalStateT repl initialState
-> take fish
I don't understand that.
λ> evalStateT repl initialState
-> take bed
There is no bed in this room.
λ> evalStateT repl initialState
-> drop armadillo
I don't understand that.
λ> evalStateT repl initialState
-> drop bed
What do you mean, drop the bed?
λ> evalStateT repl initialState
-> east
You go east.
λ> evalStateT repl initialState
-> look
You are in a small kitchen.
You see the following objects:
pot
stove
There are exits in the following directions:
north
east
south
λ> evalStateT repl initialState
-> exit
Goodbye!
*** Exception: ExitSuccess
λ> evalStateT repl initialState
-> north and north and north and north and north
You go north.
You go north.
There is no exit in that direction.
There is no exit in that direction.
There is no exit in that direction.
```

And that's it! You've written almost all the imperative code you're going to
need to run the game! Next week we'll write a short `Main` module to draw it all
together.

## 2.1 Submitting

Before submitting your project, make sure your directory looks like this:

```
$ tree .
.
├── README.md
├── app
│   └── Main.hs
├── functional-adventure.cabal
├── package.yaml
├── src
│   ├── Command.hs
│   ├── Direction.hs
│   ├── Example.hs
│   ├── GameIO.hs
│   ├── GameState.hs
│   ├── Item.hs
│   ├── Player.hs
│   └── Room.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs
```

This is required for full credit.
