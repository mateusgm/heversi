{-# OPTIONS -fglasgow-exts #-}

-- | Definition and utilities for working with the Othello game board.
module Board
where

import qualified Data.IntMap 	as Map
import Data.IntMap 		(IntMap)
import qualified Data.Foldable


-- Stone ----------------------------------------------------------------------
-- | A player's stone marker.
data Stone
	= Dark 
	| Light 
	deriving (Eq, Show)

-- | Get the opponent stone of this one.
opponent :: Stone -> Stone
opponent stone
 = case stone of
	Dark	-> Light
	Light	-> Dark


-- Board ----------------------------------------------------------------------
-- | A coordinate on the game board.
type Coord = (Int, Int)

-- | A square on the board.
data Square
	= Stone Stone		-- ^ a square filled with a player's stone.
	| Vacant		-- ^ an empty, vacant square.
	deriving (Eq, Show)


-- | The game board is a map from coordinates to squares.
data Board 
	= Board 
	{ bWidth	:: Int 		
	, bHeight	:: Int		
	, bSquares	:: IntMap Square }


-- | Check if this coord is a valid position on this board.
coordInBoard :: Coord -> Board -> Bool
coordInBoard coord@(x, y) board
 	=  x >= 0 && x < bWidth board
	&& y >= 0 && y < bHeight board

	
-- | Make a list of all the valid coordinates in this board.
allCoords :: Board -> [Coord]
allCoords board
	= [ (x, y)
		| x <- [0 .. bWidth  board - 1]
		, y <- [0 .. bHeight board - 1] ]
		
		
-- Standard Setup -------------------------------------------------------------

-- | Standard othello boards are 8x8 squares.
standardBoardSize :: Int
standardBoardSize = 8

-- | An initial board, of the standard size.
standardInitialBoard :: Board
standardInitialBoard 
	= initialBoard standardBoardSize standardBoardSize


-- Stone utils ----------------------------------------------------------------

-- | Get the map index of this coordinate.
coordIx :: Coord -> Board -> Int
coordIx (x, y) board
	= x + y * (bHeight board)


-- | Set a square in the game board.
setSquare :: Coord -> Square -> Board -> Board
setSquare coord@(x, y) square board
	| not $ coordInBoard coord board
	= error $ "setSquare: square " ++ show coord ++ " not in board."
	
	| otherwise
	= Board (bWidth board) (bHeight board)
	 	(Map.insert (coordIx coord board) square (bSquares board))

	
-- | Get the value of a square in the game board.
getSquare :: Coord -> Board -> Square
getSquare coord board
 = case Map.lookup (coordIx coord board) (bSquares board) of 
	Nothing		-> error $ "getSquare: square " ++ show coord ++ " not in board"
	Just square	-> square


-- | Count all squares of a certain state in the board.
countSquares :: Square -> Board -> Int
countSquares square board
	= Map.fold
		(\s x -> if s == square then x + 1 else x)
		0 
		(bSquares board)


-- | Flip the stones at all these coordinates to the opposite colour.
flipSquares :: [Coord] -> Board -> Board
flipSquares coords board
	= foldr flipSquare board coords

	
-- | Flip the stone at this coordinate.
flipSquare :: Coord -> Board -> Board
flipSquare coord board
 = case getSquare coord board of
	Vacant		-> board
	Stone stone	-> setSquare coord (Stone $ opponent stone) board


-- Initial Boards -------------------------------------------------------------

-- | A board with no squares at all.
emptyBoard :: Int -> Int -> Board
emptyBoard width height 
	= Board width height Map.empty

-- | A board with all vacant squares.
vacantBoard :: Int -> Int -> Board
vacantBoard width height
	= foldr (uncurry setSquare) (emptyBoard width height)
		[ ((x, y), Vacant)
			| x <- [0..width -1]
			, y <- [0..height-1] ]

-- | Create an in initial game board with the standard starting pattern.
initialBoard :: Int -> Int -> Board
initialBoard width height
	= foldr (uncurry setSquare) (vacantBoard width height)
		[ ((padX, padY),   Stone Dark),  ((padX+1, padY),   Stone Light)
		, ((padX, padY+1), Stone Light), ((padX+1, padY+1), Stone Dark) ]
	where
		padX	= (width -  2) `div` 2
		padY	= (height - 2) `div` 2
		
