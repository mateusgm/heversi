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

turn g@(Play _ x _) = x
idle g@(Play _ _ x) = x


-- play a move in a GameState
play'' :: GameState -> Move -> GameState
play'' g@(Over _ _ _) _   = g
play'' g@(Draw _) _       = g
play'' g@(Play b t i) m@(_,s)
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
        (wh, bl)  = if (isWhite t) then (t,i) else (i,t)

