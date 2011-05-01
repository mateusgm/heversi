module Models.Game.Player where

import qualified Models.Game.Board as Board (isWhite)
import Models.Game.Board                    (Stone) 


----- Player

data Player = Human Stone | AI Stone
              deriving (Show)


human :: Stone -> Player
human = Human

ai :: Stone -> Player
ai = AI


isHuman :: Player -> Bool
isHuman (Human _) = True
isHuman _         = False

isAI :: Player -> Bool
isAI = not . isHuman


stone :: Player -> Stone
stone (Human s) = s
stone (AI s)    = s

isWhite :: Player -> Bool
isWhite (Human s) = Board.isWhite s
isWhite (AI s)    = Board.isWhite s

isBlack :: Player -> Bool
isBlack = not . isWhite
