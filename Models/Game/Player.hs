module Models.Game.Player where

import Models.Game.Board  (Stone)


----- Player

data Player = Human Stone | AI Stone

stone :: Player -> Stone
stone Human s = s
stone AI s    = s

human :: Stone -> Player
human = Human

ai :: Stone -> Player
ai = AI

isHuman :: Player -> Bool
isHuman Human _ = True
isHuman _       = False

isAI :: Player -> Bool
isAI = not . isHuman



