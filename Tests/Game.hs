module Tests.Game where

import Models.Game.Board
import Models.Game.Engine
import Models.Game.Player
import Models.Game.AI

import Debug.Trace

--------- testing game model functions

mb p = (p, mkBlack)
mw p = (p, mkWhite)

test m@(Draw _ )    = m 
test m@(Over _ _ _) = m 
test m@(Game b t _) = test $ trace (show p) p 
  where p = play m $ move b $ t
  
--------- testing game model data
  
g = start  

