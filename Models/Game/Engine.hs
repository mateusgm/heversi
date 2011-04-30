module Models.Game.Rules where

import Models.Game.Board
import Models.Game.Player     (isBlack, isWhite)
import Models.Game.IA

type Turn    = Player
type Winner  = Player
type Loser   = Player
type Points  = Integer

data Match   = Active   Board Turn   Idle   |
               Finished Board Winner Loser  |
               Draw     Board Player Player


begin :: Player -> Player -> Match
begin p1 p2 = Active board p1 p2

play :: Match -> Move -> Match
play s@(Finished _ _ _) _ = s
play s@(Draw _ _ _) _     = s
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
end s@(Draw _ _ _)     = s
end s@(Active b t i)
  | isNil b            = s
  | wCount > bCount    = Finished b w b
  | wCount < wCount    = Finished b b w
  | otherwise          = Draw b t i
  where (wCount,bCount)  = score b
        (w, b) = (isWhite t) ? (t,i) : (i,t)
        
