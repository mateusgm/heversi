module Tests.Game where

import Models.Game.Board
import Models.Game.Engine
import Models.Game.Player
import Models.Game.AI
import Models.Game

import Debug.Trace
import HappStack.State        (startSystemState, shutdownSystem,
                               createCheckpoint)

--------- testing game model - functions

mb p = (p, mkBlack)
mw p = (p, mkWhite)

game m@(Draw _ )    = m 
game m@(Over _ _ _) = m 
game m@(Play b t _) = test $ trace (show p) p 
  where p = play m $ move b $ t
  
--------- testing game model - data
  
g = start  

--------- testing the state system

rootState :: Proxy GameDir
rootState = Proxy

main = do startSystemState rootState
          id1 <- update . AddGame . Game start Android $ Android
          print id1
          g1 <- query . GetGame $ 1 
          print g1
