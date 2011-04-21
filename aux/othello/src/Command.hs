
-- | Commands used to interact wih the game.
module Command where

import State
import Board

-- | A user command
data Command 
	-- non-commands
	= Blank			-- ^ no command entered.
	| Illegal		-- ^ an invalid move.
	| Huh			-- ^ a malformed command.

	-- meta-commands
	| Quit 			-- ^ quit the game.
	| Help			-- ^ print the instructions.

	-- game information
	| Display 		-- ^ display the boad.
	| Possible		-- ^ show possible moves on current board.
	| DisplayTree   Int	-- ^ display the game tree to this depth.
	| DisplayAnnot  Int	-- ^ display the annotated tree to this depth.
	| DisplayPruned Int	-- ^ display the pruned tree to this depth.

	-- manual moves
	| Move Coord		-- ^ place a stone in this coordinate.
	| Pass 			-- ^ pass the current move.

	-- | set the algorithm controlling this player.
	| SetPlayer Stone Player 
	
	-- | make a single move with this algorithm
	--	Argument should not be Manual.
	| SingleMove Player	

	deriving (Show, Eq)


-- | Usage information for each of the commands:
commandHelp :: String
commandHelp
 	= unlines
	[ "Meta commands ------------------------------------------------------"
	, "    q,  quit                         Quit the game"
	, "    h,  help                         Command help"
	, ""
	, "Basic Gameplay -----------------------------------------------------"
	, "    d,  display                      Display the board"
	, "    m <x> <y>, move <x> <y>          Place a stone"
	, "    p,  pass                         Pass this move"
	, ""
	, "Player Setup --- stone is 'O' or 'X' -------------------------------"
	, "    player <stone> manual            Manual / human player"
	, "    player <stone> greedy            Greedy algorithm"
	, "    player <stone> minimax <depth>   Minimax algorithm"
	, "    player <stone> pruned  <depth>   Minimax with pruned tree"
	, ""
	, "Make single computer moves -----------------------------------------"
	, "    s g,         single greedy"
	, "    s m <depth>, single minimax <depth>"
	, "    s r <depth>, single pruned  <depth>"
	, ""
	, "Game information ---------------------------------------------------"
	, "    o, possible                      Possible moves"
	, "    t <depth>, tree <depth>          Show game tree to this depth"
	, "    a <depth>, annot <depth>         Show annotated game tree"
	, "    r <depth>, pruned <depth>        Show pruned game tree"
	]
 
