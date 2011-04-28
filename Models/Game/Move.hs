module Models.Game.Move where

import Models.Game.Board    (Board(Board,Nil))
import Models.Game.Player


data Move = Move Player Position deriving (Show)

move      :: Board -> Move -> Board
move (Nil)      _              = Nil
move (Board b) (Move pl (x,y))
  | b!(x,y) /= None            = Nil
  | null changes               = Nil
  | otherwise                  = Board $ b // changes
  where dx                        = [1, 1, 0, -1, -1, -1, 0, 1]
        dy                        = [0, 1, 1, 1, 0, -1, -1, -1] 
        changes                   = findChanges dx dy        
        findChanges []     []     = ((x,y),pl) : []
        findChanges (i:is) (j:js) = findAxisChanges (x+i,y+j) (i,j) []
                                    ++ findChanges is js
        findAxisChanges (p,q) (dp,dq) result
          | outBounds (p,q)  = []
          | b!(p,q) == None  = []
          | b!(p,q) == pl    = result
          | otherwise        = findAxisChanges (p+dp,q+dq) (dp,dq) (((p,q),pl):result)         



