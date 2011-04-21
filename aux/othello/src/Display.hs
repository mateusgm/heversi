{-# OPTIONS -fglasgow-exts #-}

-- | Functions for displaying the game state on the screen.
module Display where

import State
import Board
import Plan
import Game
import Config
import VT100

import Data.List
import Control.Monad
import System.IO


-- | Print the game state to the screen.
printState :: State -> IO ()
printState state
 = do	-- ensure the state is completely evaluated before we clear the screen
	-- 	this prevents the screen being blank for too long
	let strState	= displayState state
	seqString strState `seq` return ()

	-- clear the screen and move the cursor back to the home position
	when configClearScreen 
	 $ do	putStr $ VT100.eraseDisplay
		putStr $ VT100.setCursorPos 1 1 

	-- print the game state
	putStr strState
	hFlush stdout

	return ()

-- | Walk over this string, making sure it's all evaluated.
seqString :: String -> ()
seqString xx
 = case xx of
	[]	-> ()
	(x:xs)	-> x `seq` seqString xs
	

-- | Display the game state as a string.
displayState :: State -> String
displayState state
 = let 	-- stone sigils.
	cX	= withStoneColor Dark
	cO	= withStoneColor Light

	-- marks showing who's turn it is.
	markX	= if sTurn state == Dark  then " >" else "  "
	markY	= if sTurn state == Light then " >" else "  "

   in	unlines
	[ ""
	, displayBoard (sLastMove state) (sBoard state)
	, ""
	, markX ++ "(" ++ cX "X" ++ ") " 
		++ padR 10 (show $ sPlayerDark state) 
	   	++ ": "	
		++ show (countSquares (Stone Dark) (sBoard state))

	, markY ++ "(" ++ cO "O" ++ ") "
		++ padR 10 (show $ sPlayerLight state)
	   	++ ": "
		++ show (countSquares (Stone Light) (sBoard state)) 
	]


-- | Display this board as a string.
displayBoard :: Maybe Coord -> Board -> String
displayBoard mLastMove board
 = let	numRow 	= "   " 
		++ (concat 
			$ intersperse " " 
			$ map show [0 .. bWidth board - 1])
		++ "\n"
			
	squareRows
		= concat 
		$ intersperse "\n"
		$ map (displayRow mLastMove board) [0 .. bHeight board - 1]

   in	numRow ++ squareRows


-- | Display a single board row as a string.
displayRow :: Maybe Coord -> Board -> Int -> String
displayRow mLastMove board row
	=  " "
	++ show row 
	++ " "
	++ (concat 
		$ intersperse " "
	 	$ [ displaySquare mLastMove (col, row) (getSquare (col, row) board)
			| col <- [0 .. bWidth board - 1] ])
	
	
-- | Display a square of the board.
displaySquare :: Maybe Coord -> Coord -> Square -> String
displaySquare mLastMove coord square
	| Vacant		<- square
	= "."
	
	| Stone stone		<- square
	, Just lastCoord	<- mLastMove
	= if coord == lastCoord
	   then	withUnderscore (withStoneColor stone (displayStone stone))
	   else	withStoneColor stone (displayStone stone)

	| Stone stone		<- square
	= withStoneColor stone (displayStone stone)

	
-- | Display a stone sigil.
displayStone :: Stone -> String
displayStone stone
 = case stone of
	Light	-> "O"
	Dark	-> "X"


-- | Display a game tree.
--	On each line, show the sequence of moves leading to the node, 
--	then the player who's move it is, current score, and the node annotation.
--
displayGameTree 
	:: Show a
	=> Stone 		-- ^ display scores relative to this stone
	-> GameTree a 		-- ^ game tree to display
	-> String		-- ^ ouput string

displayGameTree scoreStone tree
 = let	maxDepth	= depthOfTree tree
	width		= maxDepth * 6 + 2
   in	replicate width ' '
		++ "       turn score annot\n"
		++ displayGameTree' width 0 scoreStone [] tree


displayGameTree'
	:: Show a
	=> Int			-- ^ maximum depth of tree
	-> Int			-- ^ current depth
	-> Stone		-- ^ stone to calculate scores with
	-> [Maybe Coord]	-- ^ sequence of moves leading to this node
	-> GameTree a		-- ^ node of tree
	-> String		-- ^ output string
	
displayGameTree' width depth scoreStone path
	tree@(Leaf annot stone board)
 = let	score	= scoreBoard scoreStone board
   	pathStr	= padR width $ concat $ intersperse " " $ map displayPathChoice path
   in	pathStr	
		++ "  Leaf" 	
		++ " "  ++ padL 4 (displayStone stone)
		++ " "	++ padL 5 (displayInt score)
		++ " " 	++ padL 5 (show annot)
		++ "\n"	

displayGameTree' width depth scoreStone path
	tree@(Node annot stone board choices)
 = let	score	= scoreBoard scoreStone board
   	pathStr	= padR width $ concat $ intersperse " " $ map displayPathChoice path
   in	pathStr 
		++ "  Node"	
		++ " "  ++ padL 4 (displayStone stone)
		++ " " 	++ padL 5 (displayInt score) 
		++ " "	++ padL 5 (show annot)
		++ "\n"
		++ (concatMap (displayGameTree_choice width depth scoreStone path) choices)

displayGameTree_choice width depth scoreStone path choice
 = case choice of
	ChoiceMove coord tree'
	 ->	displayGameTree' width (depth + 1) scoreStone (path ++ [Just coord]) tree'
	
	ChoicePass tree'
	 ->	displayGameTree' width (depth + 1) scoreStone (path ++ [Nothing]) tree'

displayPathChoice :: Maybe Coord -> String
displayPathChoice mc
 = case mc of
	Nothing	-> "-----"
	Just c	-> show c


-- | Pad out the right of a string with spaces so it's a certain number of charaters wide.
padR :: Int -> String -> String
padR width str
	= str ++ replicate (width - length str) ' '


-- | Pad out the left of a string with spaces so it's a certain number of charaters wide.
padL :: Int -> String -> String
padL width str
	= replicate (width - length str) ' ' ++ str


-- | Display a signed int, aligned to the left
displayInt :: Int -> String
displayInt i
	| i < 0		= show i
	| otherwise	= " " ++ show i


-- | Add VT100 codes to make this string come out in the color associated with this stone.
withStoneColor :: Stone -> String -> String
withStoneColor stone str
	| configColor
	, mode	<- case stone of
			Light	-> [Bold, Foreground Blue]
			Dark	-> [Bold, Foreground Red]

	= setMode mode ++ str ++ setMode [Reset]

	| otherwise
	= str


-- | Add VT100 codes to make this string come out underscored.
withUnderscore :: String -> String
withUnderscore str
	| configColor	= setMode [Underscore] ++ str ++ setMode [Reset]
	| otherwise	= str

