
-- | The overall game state.
module State
where

import Board

-- | Possible player algorithms.
data Player
	= Manual 			-- human player / manual moves			
	| Greedy			-- computer using the greedy algorithm
	| Minimax Int			-- computer using the minimax algorithn, depth
	| Pruned  Int			-- computer using the pruned minimax algorithm, depth
	deriving (Show, Eq)


-- | Carries the overall state of the game
data State
	= State 
	
	{ sBoard 	:: Board	-- the current board
	, sTurn		:: Stone	-- the player who's turn it is	
	, sTurnCount	:: Int		-- how many turns have been played so far
	, sPlayerLight	:: Player	-- what is controlling the light player
	, sPlayerDark	:: Player 	-- what is controlling the dark player

	-- what move the last player made
	--	Nothing if the player passed their move
	, sLastMove	:: Maybe Coord
	}
 		

-- | Use the state to work out what's controlling the player with this stone color.
getPlayerOfStone :: State -> Stone -> Player
getPlayerOfStone state stone
 = case stone of
	Light	-> sPlayerLight state
	Dark	-> sPlayerDark  state


-- | Flip the turn in this state, and advance the turn count.
advanceState :: State -> State
advanceState state
	= state 
	{ sTurn		= opponent (sTurn state) 
	, sTurnCount	= sTurnCount state + 1 }
	
	