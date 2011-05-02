module Models.Game where

import Models.Game.Board
import Models.Game.Engine
import Models.Game.Player
import Models.Game.Stone
import Models.Game.AI

import Debug.Trace

--------- teste engine

h1 = human black
h2 = human white
ia = ai white

m1 = begin h1 h2
m2 = begin h1 ia

x1 = ((6,4), black)
x2 = ((6,5), black)

test m@(Draw     _ _ _) = m 
test m@(Finished _ _ _) = m 
test m@(Active   b t _)   = test $ trace (show p) p 
  where p = play m $ move b $ stone t


