module Models.Game.Engine
  where

import Prelude                 hiding (flip)
import Models.Game.Player     (Turn, Idle, Winner, Loser,
                               white, mkWhite, mkBlack)
import Models.Game.Board      (Board, Move, board, count, prospects,
                               flips, flip)


-- the GameState
data GameState = Game Board Turn   Idle  |
                 Over Board Winner Loser |
                 Draw Board
                 deriving (Show)


-- begin a match between two players
start = Game board mkBlack mkWhite

-- play a move in a GameState
play :: GameState -> Move -> GameState
play g@(Over _ _ _) _   = g
play g@(Draw _) _       = g
play g@(Game b t i) m@(_,s)
  | t /= s              = g
  | valid m             = g 
  | blocked i           = if (blocked t) then (over g) else (game t i) 
  | otherwise           = game i t
  where flips'  = flips b
        valid   = null . flips'
        board'  = flip b . flips' $ m
        blocked = null . prospects board' 
        game    = Game board'
        
-- end a match
over :: GameState -> GameState
over g@(Over _ _ _) = g
over g@(Draw _)     = g
over g@(Game b t i)
  | count' wh > count' bl = Over b wh bl
  | count' wh < count' bl = Over b bl wh
  | otherwise             = Draw b
  where count' = count b
        (wh, bl)  = if (white t) then (t,i) else (i,t)

