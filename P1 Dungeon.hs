--The dungeon thing

{-
	In this game, the player will traverse a fictional location called the "dungeon", with the goal
	of reaching the exit alive.

	Along the way, the player will encounter obstacles--traps, monsters, dangerous event etc.--which
	will hinter the player's progress. The player can also find items and interactions which assists
	the escape.

	The player's physical and mental condition will be represented by values such as "Health", "Courage"
	and "Sobriety". If any of the vital statuses reach 0, the player will lose and be forced to restart.

	Harmful events such as falling into a trap, being attacked by monster etc. will reduce the player's
	vital statuses, while helpful events will restore them.

	The dungeon is divided into rooms, and the player will navigate among them to find the exit. When
	entering a new room, the player may encounter the dangerous or helpful events described above.
	The player will not be aware of the contents of each room before entering them, baring exceptional
	events which may reveal unvisited rooms.

	When encountering an event, the player may choose from a finite number of different possible actions
	in response, each with distinct outcome which will affect the player's vital statuses and possibly
	add/remove items from the player's inventory.
-}


module Dungeon where

import System.IO
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
--import System.Random !!! cannot be found!

-- return True if input contains 'y' or 'Y'
saidYes speech = (elem 'y' speech) || (elem 'Y' speech)

-- ask the user to enter an integer, if failed, retry until success; 
-- returns the input integer
lp_reqInt prompt = do
  if (prompt=="") then putStr "" else putStrLn prompt
  input1 <- getLine
  case readMaybe input1 of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      lp_reqInt prompt
    Just n -> return n

-- ask the user to enter an integer (in a range defined by inclusive min. and max.)
-- if failed, retry until success; returns the input integer
lp_reqIntR prompt imin imax = do
  if (prompt=="") then putStr "" else putStrLn prompt
  input1 <- getLine
  case readMaybe input1 of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      lp_reqIntR prompt imin imax
    Just n -> if ((n<imin) || (n>imax)) 
              then do
                putStrLn ("(input must be within "++show imin++"~"++show imax++")")
                lp_reqIntR prompt imin imax
              else return n

-- ask the user to enter a string, if string matches a certain condition,
-- return the input string, if not, return an error message and ask again
lp_reqStrCond prompt errMsg cond = do
  if (prompt=="") then putStr "" else putStrLn prompt
  input1 <- getLine
  case (cond input1) of
    False -> do
      putStrLn errMsg
      lp_reqStrCond prompt errMsg cond
    True -> return input1

-- get a random integer within range
-- !!!cannot be done: randomInt imin imax = getStdRandom (randomR (imin, imax))


-- Test number 1
beginTest = testLoop "" 7

testLoop ws ap = do
  putStrLn ws
  if ap == 0 then return() else do
    putStrLn "What will you say today?"
    speech <- getLine
    putStrLn "What will happen in the world?"
    change <- getLine
  
    testLoop (ws ++ "God said: " ++ "'" ++ speech ++ "', and " ++ change ++ "\n") (ap-1)


-- Test number 2
beginWalk = roomWalk "middle"

roomWalk ws = do
  putStrLn "You are at the " ++ ws ++ " of the room. Where will you go?"
  case ws of
    "left" -> 