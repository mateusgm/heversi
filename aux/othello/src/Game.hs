{-# OPTIONS -fglasgow-exts #-}

-- | High-level functions for playing the Othello game.
module Game where

import Board
import State
import Data.Maybe


-- Scoring --------------------------------------------------------------------
-- | Get a +ve or -ve score relative to this stone
scoreBoard :: Stone -> Board -> Int
scoreBoard stone board
 = let	countLight	= countSquares (Stone Light) board
	countDark	= countSquares (Stone Dark)  board
   in	case stone of
		Dark  -> countDark - countLight
		Light -> countLight - countDark


-- Stone Capturing ------------------------------------------------------------

-- | Work out the coordinates of the stones that will be captured by 
--	placing a new one here.
capture :: Coord -> Stone -> Board -> [Coord]
capture coord stone board 
 	= concat 
	$ [ captureInDirection coord dir stone board
	  	| dir  <- [ ( 1,  0), (-1,  0), ( 0, 1),  ( 0, -1)
			  , ( 1,  1), ( 1, -1), (-1, 1),  (-1, -1) ] ]


-- | A vector direction on the board.
--	Each component must be -1, 0 or +1
type Direction = (Int, Int)

-- | See which stones can be captured in a certain direction by placing
--	a new stone at this coordinate.
captureInDirection 
	:: Coord 		-- ^ coordinate of new stone.
	-> Direction 		-- ^ direction of capturing.
	-> Stone 		-- ^ color of new stone.
	-> Board 		-- ^ current game board.
	-> [Coord]		-- ^ coordinates of captured stones.

captureInDirection coord@(x, y) dir@(dx, dy) stone board
	-- we can't start off the edge of the board
	| not $ coordInBoard coord board
	= []
	
	-- we can't start in a coord that already has a stone
	| Stone _ 	<- getSquare coord board
	= []
	
	| otherwise
	= fromMaybe [] 
	$ sequence 
	$ captureWalkInDirection 
		(x + dx, y + dy) 
		dir stone board 
	
captureWalkInDirection 
	:: Coord -> Direction -> Stone -> Board -> [Maybe Coord]

captureWalkInDirection coord@(x, y) dir@(dx, dy) stone board

	-- If we've walked off the edge of the board without finding the
	--	end stone then no stones can be captured.
	| not $ coordInBoard coord board
	= [Nothing]

	-- There's a stone at this position
 	| Stone stoneHere	<- getSquare coord board
	= if stoneHere == stone
	   -- we've found the end stone, so return [] to say we're done walking.
	   then []
	   -- it's the opponents stone, so add it to the list and keep walking.
	   else Just coord : captureWalkInDirection (x + dx, y + dy) dir stone board

	-- This square was vacant, so no stones can be captured.
	| otherwise
	= [Nothing]


-- Placing --------------------------------------------------------------------
-- | Try to place a stone at this coordinate
placeStone 
	:: Stone 		-- ^ color of stone to place.
	-> Coord		-- ^ coord of stone.
	-> Board		-- ^ current board.
	-> Maybe 		--   Nothing if this is not a valid move,
		( [Coord]	-- 	opponent stones captured,
		, Board)	-- 	new board	

placeStone stone coord board
 = case capture coord stone board of
	[] 
	 -> Nothing

	captured
	 -> let	board'	= setSquare coord (Stone stone)
			$ flipSquares captured board
	    in	Just (captured, board')
	
	
-- Valid Moves ----------------------------------------------------------------
-- | Work out the valid moves for a this player
validMoves 
	:: Stone 		-- ^ stone of active player
	-> Board 		-- ^ game board
	-> [( Coord		--   coord of new stone,
	    , [Coord]		-- 	opponent stones captured,
	    , Board)]		-- 	new game board
	
validMoves stone board
 = 	[ (coord, captured, board')
		| coord				<- allCoords board 
		, let mCapBoard			= placeStone stone coord board
		, isJust mCapBoard
		, let Just (captured, board')	= mCapBoard ]
