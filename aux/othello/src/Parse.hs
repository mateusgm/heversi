{-# OPTIONS -fglasgow-exts #-}

-- | Parsing of user commands.
module Parse where

import Command
import State
import Board

import Data.Char

-- | Parse user request.
parseCmd :: Board -> String -> Command
parseCmd board "" = Blank
parseCmd board request
	| cmd == "quit"		|| cmd == "q"	= Quit
	| cmd == "help"		|| cmd == "h"	= Help
	| cmd == "display"	|| cmd == "d"	= Display
	| cmd == "possible"	|| cmd == "o"	= Possible

	|   cmd == "tree"	|| cmd == "t"	
	, Just cmd	<- parseTree DisplayTree args
	= cmd

	|   cmd == "annot"	|| cmd == "a"
	, Just cmd	<- parseTree DisplayAnnot args
	= cmd

	|   cmd == "pruned"	|| cmd == "r"	
	, Just cmd	<- parseTree DisplayPruned args
	= cmd

	| cmd == "move"		|| cmd == "m"
	, Just cmd	<- parseMove board args
	= cmd

	| cmd == "pass"          || cmd == "p"	
	= Pass

	| cmd == "player" 			
	, Just cmd	<- parseSetPlayer args
	= cmd

	| cmd == "single"	|| cmd == "s"
	, Just player	<- parsePlayer args
	= SingleMove player

	| otherwise				
	= Huh
	where (cmd : args) = words request


-- | Parse request for display of a game tree.
parseTree :: (Int -> Command) -> [String] -> Maybe Command
parseTree ctor args
	| [strN]	<- args
	, Just n	<- parseInt strN
	= Just (ctor n)
	
parseTree _ _
	= Nothing


-- | Parse a move command.
parseMove :: Board -> [String] -> Maybe Command
parseMove board [strX, strY]
	| Just x	<- parseInt strX
	, Just y	<- parseInt strY
	= if   x >= 0 && x < bWidth board
	    && y >= 0 && y < bHeight board
	  	then Just (Move (x, y))
		else Just (Illegal)

parseMove board _ 
	= Nothing


-- | Parse a player configuration command.
parseSetPlayer :: [String] -> Maybe Command
parseSetPlayer (strStone : argsPlayer)
	| Just stone	<- parseStone strStone
	, Just player	<- parsePlayer argsPlayer
	= Just (SetPlayer stone player)

parseSetPlayer _ 
	= Nothing


-- | Parse a player configuration.
parsePlayer :: [String] -> Maybe Player
parsePlayer args
	| [a]			<- args
	, elem a ["manual", "m"]
	= Just Manual

	| [a]			<- args
	, elem a ["greedy", "g"]
	= Just Greedy

	| ["minimax", strDepth]	<- args
	, Just depth		<- parseInt strDepth
	= Just (Minimax depth)

	| ["m", strDepth]	<- args
	, Just depth		<- parseInt strDepth
	= Just (Minimax depth)

	| ["pruned", strDepth]	<- args
	, Just depth		<- parseInt strDepth
	= Just (Pruned depth)

	| ["r", strDepth]	<- args
	, Just depth		<- parseInt strDepth
	= Just (Pruned depth)

parsePlayer _
	= Nothing 


-- | Parse a stone name.
parseStone :: String -> Maybe Stone
parseStone stone
 = case stone of
	"X"	-> Just Dark
	"x"	-> Just Dark
	"O"	-> Just Light
	"o"	-> Just Light
	_	-> Nothing


-- | Parse an integer.
parseInt :: String -> Maybe Int
parseInt str
	| all isDigit str	= Just (read str)
	| otherwise		= Nothing


