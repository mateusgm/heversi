{-# OPTIONS -XPatternGuards #-}

-- | Handles updating of the game state to implement the computer's moves.
module Play
where

import State
import Plan
import Board
import Game
import Board
import Random

import Data.Function
import Data.List

-- | Play a move with one of the computer algorithms.
playComputerMove 
	:: Stone 		-- ^ stone color of computer player.
	-> Player		-- ^ algorithm to use.
	-> State 		-- ^ old state.
	-> IO State		-- ^ new state.

playComputerMove turn player state
 = case player of
	Greedy		-> playMoveGreedy turn state
	Minimax depth	-> playMoveMinimax depth turn state
	Pruned  depth	-> playMovePruned  depth turn state
	

-- | Play a move with the greedy algorithm.
playMoveGreedy 
	:: Stone 		-- ^ stone color of computer player.
	-> State 		-- ^ old game state.
	-> IO State		-- ^ new game state.

playMoveGreedy 
	= error "Play.playMoveGreedy: not implemented"


-- | Play a move with the minimax algorithm.
playMoveMinimax 
	:: Int 			-- ^ use this game tree depth.
	-> Stone 		-- ^ stone of computer player.
	-> State 		-- ^ old game state.
	-> IO State		-- ^ new game state.

playMoveMinimax 
	= error "Play.playMoveMinimax: not implemented"


-- | Play a move with the pruned minimax algorithm.
playMovePruned 
	:: Int 			-- ^ use this game tree depth.
	-> Stone 		-- ^ stone of computer player.
	-> State 		-- ^ old game state.
	-> IO State		-- ^ new game state.

playMovePruned
	= error "Play.playMovePruned: not implemented"
