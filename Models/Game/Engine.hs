module Models.Game.Engine where

import Models.Game.Board      hiding (isBlack, isWhite)
import Models.Game.Player     (Player, isBlack, isWhite, stone, isHuman)
import Models.Game.IA
import Debug.Trace

type Turn    = Player
type Idle    = Player
type Winner  = Player
type Loser   = Player

data Match   = Active   Board Turn   Idle   |
               Finished Board Winner Loser  |
               Draw     Board Player Player
               deriving (Show)


begin :: Player -> Player -> Match
begin p1 p2 = Active board p1 p2

play :: Match -> Move -> Match
play s@(Finished _ _ _) _ = s
play s@(Draw _ _ _) _     = s
play s@(Active b t i) m@(_,st)
  | stone t /= st      = s
  | isNil newBoard     = s
  | null $ prospects i = if (null $ prospects t) then (end s)
                          else (newState i t)    
  | otherwise          = newState t i
  where newBoard       = move b m
        prospects      = getProspects newBoard . stone
        newState tx ix
          | isHuman ix = state 
          | otherwise  = trace (show state) $ play state moveAI
          where state  = Active newBoard ix tx
                moveAI = getMove newBoard $ stone ix

end :: Match -> Match
end s@(Finished _ _ _) = s
end s@(Draw _ _ _)     = s
end s@(Active b t i)
  | isNil b            = s
  | wCount > bCount    = Finished b white black
  | wCount < wCount    = Finished b black white
  | otherwise          = Draw b t i
  where (wCount,bCount)  = score b
        (white, black) = if (isWhite t) then (t,i) else (i,t)
        
