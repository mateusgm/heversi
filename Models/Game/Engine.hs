module Models.Game.Engine
   (module Models.Game.Player,
    module Models.Game.Board,
    GameState(..), start, play, playAI, turn
   ) where

import Prelude                 hiding (flip)

import Models.Types           (GameState(..)) 
import Models.Game.AI         (move)
import Models.Game.Player     (white, mkWhite, mkBlack)
import Models.Game.Board      (Move, board, count, prospects,
                               flips, flip)


-- begin a match between two players
start = Play board mkBlack mkWhite

turn g@(Play _ t _) = t

playAI :: GameState -> GameState
playAI s@(Play b t i) = play s . move b $ t 

-- play a move in a GameState
play :: GameState -> Move -> GameState
play g@(Over _ _ _) _   = g
play g@(Draw _) _       = g
play g@(Play b t i) m@(_,s)
  | t /= s              = g
  | valid m             = g 
  | blocked i           = if (blocked t) then (over g) else (game t i) 
  | otherwise           = game i t
  where flips'  = flips b
        valid   = null . flips'
        board'  = flip b . flips' $ m
        blocked = null . prospects board' 
        game    = Play board'
        
-- end a match
over :: GameState -> GameState
over g@(Over _ _ _) = g
over g@(Draw _)     = g
over g@(Play b t i)
  | count' wh > count' bl = Over b wh bl
  | count' wh < count' bl = Over b bl wh
  | otherwise             = Draw b
  where count' = count b
        (wh, bl)  = if (white t) then (t,i) else (i,t)

