{-# OPTIONS -fglasgow-exts #-}

-- | Planning of the next move, using variations of the minimax algorithm.
module Plan where

import Board
import Game

import Data.Maybe
import Data.Function
import Data.List

-- | The game tree records the possible game states starting from a particular board.
data GameTree a
	= Leaf	{ tAnnot	:: a 		-- ^ annotation on this leaf.
		, tStone	:: Stone	-- ^ stone of player who's turn it is for the contained board.
		, tBoard	:: Board }	-- ^ the board.	

	| Node 	{ tAnnot	:: a		-- ^ annotation on this node.
		, tStone	:: Stone 	-- ^ stone of the player who's turn it is for the contained board.
		, tBoard	:: Board 	-- ^ the board.
		, tChoices	:: [Choice a] }	-- ^ possible choices the player could make from this board.
						--	Either a list of ChoiceMove's or a single ChoicePass.
	
-- | Represents a choice the player could make.
data Choice a
	= ChoiceMove 
		{ cCoord	:: Coord 	-- ^ coordinate of the new stone.
		, cTree		:: GameTree a }	-- ^ next node in the tree.

	| ChoicePass
		{ cTree		:: GameTree a }	-- ^ the next node in the tree.


-- | Get the annotation on the tree in this choice.
annotOfChoice :: Choice a -> a
annotOfChoice choice	= tAnnot (cTree choice)


-- | The maximum depth of this tree.
depthOfTree :: GameTree a -> Int
depthOfTree tree
 = case tree of
	Node _ _ _ choices	-> maximum $ map ((+ 1) . depthOfChoice) choices
	Leaf _ _ _		-> 0
	
depthOfChoice choice
 = case choice of
	ChoiceMove _ tree	-> depthOfTree tree
	ChoicePass tree		-> depthOfTree tree


-- | The number of nodes and leaves in this tree.
sizeOfTree :: GameTree a -> Int
sizeOfTree tree
 = case tree of
	Node _ _ _ choices	-> 1 + (sum $ map sizeOfChoice choices)
	Leaf _ _ _		-> 1

sizeOfChoice choice
 = case choice of
	ChoiceMove _ tree	-> sizeOfTree tree
	ChoicePass tree		-> sizeOfTree tree


-- | Select the choices with the highest annotation from this list.
chooseBest :: [Choice Int] -> [Choice Int]
chooseBest = error "Plan.chooseBest: not implemented"


-- | Select the choices with the lowest annotation from this list.
chooseWorst :: [Choice Int] -> [Choice Int]
chooseWorst = error "Plan.chooseWorst: not implemented"


-- Build ----------------------------------------------------------------------
-- | Build a game tree to a certain depth, 
--	setting the annotation to the unit value ().
buildGameTree 
	:: Int 			-- ^ depth of tree.
	-> Stone 		-- ^ stone of the current player.
	-> Board 		-- ^ the starting board.
	-> GameTree ()
	
buildGameTree = error "Plan.buildGameTree: not implemented"


-- Annot ----------------------------------------------------------------------
-- | Annotate the game tree with the minimum possible score for each branch.
annotateTree
	:: Stone		-- ^ stone of the controlling player.
	-> GameTree ()		-- ^ unannotated choice
	-> GameTree Int		-- ^ annotated choice

annotateTree = error "Plan.annotateTree: not implemented"


-- | Annotate the game tree contained within this choice
annotateChoice 
	:: Stone 		-- ^ stone of the controlling player.
	-> Choice () 		-- ^ unannotaed choice
	-> Choice Int		-- ^ annotated choice

annotateChoice = error "Plan.annotateChoice: not implemented"


-- Prune ----------------------------------------------------------------------
-- | For nodes that this player has control over, prune the choices that
--	don't maximise the minimum possible score.
pruneTree 
	:: Stone		-- ^ stone of the controlling player
	-> GameTree Int		-- ^ unpruned tree
	-> GameTree Int		-- ^ pruned tree

pruneTree = error "Plan.pruneTree: not implemented"


-- | Prune the tree contained within this choice.
pruneChoice 
	:: Stone 		-- ^ stone of controlling player
	-> Choice Int 		-- ^ choice containing unpruned tree
	-> Choice Int		-- ^ choice containing pruned tree

pruneChoice = error "Plan.prunChoice: not implemented"


-- | Prune the choices in this node.
pruneNode 
	:: Stone 		-- ^ stone of controlling player
	-> GameTree Int 	-- ^ unpruned tree
	-> GameTree Int		-- ^ pruned tree

pruneNode = error "Plan.pruneNode: not implemented"

