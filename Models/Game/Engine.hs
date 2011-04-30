module Models.Game.Rules where

import Models.Game.Board
import Models.Game.Player
import Models.Game.IA

type Turn    = Player
type Winner  = Stone
type Points  = Integer

data Match   = Active   Board Turn   Idle         |
               Finished Board Winner Loser Points


start :: Player -> Player -> Match
start p1 p2 = Active board p1 p2

play :: Match -> Move -> Match
play m@(Finished _ _ _ _) _ = m
play s@(Match b t i) m
  | isNil newBoard     = s
  | null $ prospects i = if (null $ prospects t) then end s
                          else newState i t    
  | otherwise          = newState t i
  where newBoard       = move b m
        prospects      = getProspects newBoard
        newState tx ix
          | isHuman ix = state 
          | otherwise  = play state moveAI
          where state  = Match newBoard ix tx
                moveAI = getMove newBoard ix

end :: State -> State
