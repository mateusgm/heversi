module Models.Game where

import Models.Game.Board
import Models.Game.Engine
import Models.Game.Player
import Data.Array.Diff

--------- teste engine

h1 = human Black
h2 = human White
ia = ai White

m1 = begin h1 h2
m2 = begin h1 ia

x1 = ((6,4), Black)
x2 = ((6,5), Black)



