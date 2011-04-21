{-# OPTIONS -fglasgow-exts #-}

-- | Top level user interaction.
module Main where

import State
import Parse
import Command
import Display
import Config
import Play
import Board
import Game
import Plan
import VT100
import Input

import Data.Char
import Data.List
import Data.Function
import System.IO
import Control.Monad

-- | Let's play othello!
main :: IO ()
main 
 = do	-- setup the initial game state
	let initialState
		= State
		{ sBoard	= standardInitialBoard
		, sTurn		= Light
		, sTurnCount	= 0
		, sLastMove	= Nothing
		, sPlayerLight	= Manual
		, sPlayerDark	= Manual }

	-- print the initial game state
	printState initialState

	-- print the hello message
	putStr   "\n"
	putStrLn "Let's play Othello!"
	putStrLn "Type 'help' for help."
	putStr   "\n"

	-- start the game
	playPrompt initialState

-- Play -----------------------------------------------------------------------
-- | Play a turn of the game
play :: State -> IO ()
play state
 = do	-- print the game state
	printState state
	putStr "\n"
	playPrompt state

playPrompt state
	| Manual	<- getPlayerOfStone state (sTurn state)
	= do	-- display the game prompt
		putStr 	$ " " ++ show (sTurnCount state) ++ " "
			++ withStoneColor (sTurn state) (displayStone (sTurn state)) 
			++ " > "
		hFlush stdout

		-- get a command from the player
		let getLineAlt	
			| configAlternateGetLine
			= do	Just line <- getPlayerLine
				return line

			| otherwise
			= getLine

		line <- getLineAlt
		let cmd = parseCmd (sBoard state) line

		if  cmd             == Pass 
		 && sLastMove state == Nothing 
		 && sTurnCount state > 0
			then quitGameOver state 
			else execute cmd state

	| otherwise
	= do	let lastMove	= sLastMove state
		state' <- playComputerMove 
				(sTurn state) 
				(getPlayerOfStone state (sTurn state)) 
				state

		if  lastMove         == Nothing 
		 && sLastMove state' == Nothing
		 && sTurnCount state' > 0
			then quitGameOver state' 
			else play         state' 


-- Execute --------------------------------------------------------------------
-- | Execute a player's command
execute :: Command -> State -> IO ()

-- non-commands
execute Huh state
 = do 	putStrLn "  Invalid request: type 'help' for instructions"
	putStr "\n"
	playPrompt state

execute Illegal state
 = do 	putStrLn "  That is not a valid move."
	putStr "\n"
	playPrompt state

execute Blank state
 = 	playPrompt state

-- quit the game
execute Quit state 
 =	putStrLn "Bye."

-- show command help
execute Help state
 = do	putStr commandHelp
	putStr "\n"
	playPrompt state

-- redraw board
execute Display state
 = do	-- the play fn will redraw then board when it runs
	play state

-- show possible moves
execute Possible state
 = do	let valid	= validMoves (sTurn state) (sBoard state)
	putStrLn 
		$ concat 
		$ intersperse "\n" 
		$ [ show coord ++ ": " ++ (show $ length captured)
			| (coord, captured, board) <- valid]
	putStr "\n"
			
	playPrompt state

-- display the game tree
execute (DisplayTree depth) state
 = do	let tree@(Node _ _ _ choices)	
		= buildGameTree depth (sTurn state) (sBoard state)

	putStrLn $ displayGameTree (sTurn state) tree
	putStrLn $ "size = " ++ (show $ sizeOfTree tree)
	
	playPrompt state

-- display the annotated game tree
execute (DisplayAnnot depth) state
 = do	let tree@(Node _ _ _ choices)
		= buildGameTree depth (sTurn state) (sBoard state)

	let treeAnnot = annotateTree (sTurn state) tree

	putStrLn $ displayGameTree (sTurn state) treeAnnot
	putStrLn $ "size = " ++ (show $ sizeOfTree treeAnnot)

	playPrompt state

-- display the pruned game tree
execute (DisplayPruned depth) state
 = do	let tree@(Node _ _ _ choices)	
		= buildGameTree depth (sTurn state) (sBoard state)
		
	let treeAnnoted = annotateTree (sTurn state) tree
	let treePruned  = pruneTree (sTurn state) treeAnnoted
	
	putStrLn $ displayGameTree (sTurn state) treePruned
	putStrLn $ "size = " ++ (show $ sizeOfTree treePruned)
	
	playPrompt state

-- manual moves
execute (Move coord) state

	-- ensure that the square at this coordinate is vacant
	| getSquare coord (sBoard state) == Vacant
	
	-- we can only make a move if it will capture some of the opponents stones
	, yield	<- capture coord (sTurn state) (sBoard state)
	, yield /= []

	= do	-- make the new board by flipping captured stones and then
		-- placing our new one.
		let board' 	
			= setSquare coord (Stone (sTurn state))
			$ flipSquares yield (sBoard state)
	
		-- work out the new game state	
		play state
			{ sBoard	= board'
			, sLastMove	= Just coord
			, sTurn		= opponent (sTurn state) 
			, sTurnCount	= sTurnCount state + 1 }

	-- the attempted move wasn't valid for some reason
	| otherwise 
	= do	putStrLn $ "  That is not a valid move."
		putStrLn $ "  coord  = " ++ show coord
		putStrLn $ "  square = " ++ (show $ getSquare coord (sBoard state))
		putStrLn $ "  yield  = " ++ (show $ capture coord (sTurn state) (sBoard state))
		putStr "\n"
		playPrompt state
	

-- pass this turn
execute Pass state
 = do	putStr (displayState state)
	play state
		{ sTurn 	= opponent (sTurn state) 
		, sLastMove	= Nothing
		, sTurnCount	= sTurnCount state + 1 }


-- set the algorithm controlling this player
execute (SetPlayer stone player) state
 = do	let state'
		= case stone of
			Dark	-> state { sPlayerDark = player }
			Light	-> state { sPlayerLight = player }
	play state'

-- make a single computer move
execute (SingleMove player) state 
 = do	let lastMove	= sLastMove state
	state' <- playComputerMove (sTurn state) player state

	if  lastMove        == Nothing 
	 && sLastMove state' == Nothing
	 && sTurnCount state' > 0
		then quitGameOver state' 
		else play 	  state' 

-- Game Over ------------------------------------------------------------------
-- | game over man, game over
quitGameOver state
 = do	-- check who won
	let darkCount	= countSquares (Stone Dark)  (sBoard state)
	let lightCount	= countSquares (Stone Light) (sBoard state)

	let winner 
		| darkCount  > lightCount = withStoneColor Dark  (show Dark)
		| lightCount > darkCount  = withStoneColor Light (show Light)
		| otherwise    = "Nobody"

	-- congratulate the winner
	putStrLn "\nGame over."
	putStrLn $ winner ++ " wins after " ++ show (sTurnCount state) ++ " turns!"

