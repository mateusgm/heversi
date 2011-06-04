{-# LANGUAGE DeriveDataTypeable #-}
module Models.Game.Engine where

import Models.Game.AI
import Models.Game.Stone 
import Models.Game.Board

import Prelude hiding (flip)
import Data.Data      (Data, Typeable)


data GameState = Play { sBoard :: Board,
                        sTurn  :: Turn,
                        sIdle  :: Idle }  |
                 Over { sBoard :: Board,
                        sWinner :: Winner,
                        sLoser :: Loser } |
                 Draw { sBoard :: Board }
                 deriving (Eq, Show, Data, Typeable)


-- begin a match between two players
start = Play startBoard black white

turn (Play _ x _) = x
turn (Over _ x _) = x
turn (Draw _)     = black

idle (Play _ _ x) = x
idle (Over _ _ x) = x
idle (Draw _)     = white

status (Play _ _ _) = "play"
status (Over _ _ _) = "over"
status (Draw _)     = "draw"

-- play a move in a GameState
play'' :: GameState -> Move -> GameState
play'' g@(Over _ _ _) _   = g
play'' g@(Draw _) _       = g
play'' g@(Play b t i) m@(_,s)
  | t /= s              = g
  | valid m             = g 
  | blocked i           = if (blocked t) then (over g board') else (game t i) 
  | otherwise           = game i t
  where flips'  = flips b
        valid   = null . flips'
        board'  = flip b . flips' $ m
        blocked = null . prospects board' 
        game    = Play board'
        
-- end a match
over :: GameState -> Board -> GameState
over g@(Over _ _ _) _ = g
over g@(Draw _) _     = g
over g@(Play _ t i) b
  | count' wh > count' bl = Over b wh bl
  | count' wh < count' bl = Over b bl wh
  | otherwise             = Draw b
  where count' = count b
        (wh, bl)  = if (isWhite t) then (t,i) else (i,t)

