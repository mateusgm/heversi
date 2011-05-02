module Models.Game.Engine     (Match(Active, Finished, Draw),
                               begin, play, end)
  where

import Models.Game.Stone      (white, black)
import Models.Game.Player     (Player, isBlack, isWhite, isHuman, stone)
import Models.Game.Board      (Board, Move, score,
                               board, change, changes, prospects)

-- some definitions

type Turn    = Player
type Idle    = Player
type Winner  = Player
type Loser   = Player

-- the Match abstract data type

data Match   = Active   Board Turn   Idle   |
               Finished Board Winner Loser  |
               Draw     Board Player Player
               deriving (Show)

-- begin a match between two players

begin :: Player -> Player -> Match
begin p1 p2 = Active board p1 p2

-- play a move in a match

play :: Match -> Move -> Match
play s@(Finished _ _ _) _ = s
play s@(Draw _ _ _) _     = s
play s@(Active b t i) m@(_,st)
  | stone t /= st       = s
  | null changes'       = s
  | null $ prospects' i = if (null $ prospects' t) then (end s)
                          else (match' t i)    
  | otherwise           = match' i t
  where changes'        = changes b m
        board'          = change b $ changes'
        prospects'      = prospects board' . stone
        match'          = Active board'

-- end a match

end :: Match -> Match
end s@(Finished _ _ _)  = s
end s@(Draw _ _ _)      = s
end s@(Active b t i)
  | wCount > bCount     = Finished b white black
  | wCount < wCount     = Finished b black white
  | otherwise           = Draw b t i
  where (wCount,bCount) = score b
        (white, black)  = if (isWhite t) then (t,i) else (i,t)
        
