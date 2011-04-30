module Models.Game.Rules where

import Models.Game.Board
import Models.Game.Player
import Models.Game.IA

type Turn    = Player
type Winner  = Player
type Points  = Integer

data Match   = Active   Board Turn   Idle  |
               Finished Board Winner Loser 


begin :: Player -> Player -> Match
begin p1 p2 = Active board p1 p2

play :: Match -> Move -> Match
play s@(Finished _ _ _) _ = s
play s@(Active b t i) m
  | isNil newBoard     = s
  | null $ prospects i = (null $ prospects t) ? (end s)
                          : (newState i t)    
  | otherwise          = newState t i
  where newBoard       = move b m
        prospects      = getProspects newBoard . stone
        newState tx ix
          | isHuman ix = state 
          | otherwise  = play state moveAI
          where state  = Active newBoard ix tx
                moveAI = getMove newBoard $ stone ix

end :: State -> State
end s@(Finished _ _ _) = s
end (Active b t i)     = Finished b w l
  where (w,l)          = isStone t (winner b) ? (t,i) : (i,t)
