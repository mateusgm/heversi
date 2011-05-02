module Models.Game.Player    (Player(..), human, ai, stone,
                              isHuman, isAI, isWhite, isBlack)
  where

import qualified
  Models.Game.Stone as Stone (isWhite)
import Models.Game.Stone     (Stone) 


data Player = Human Stone | AI Stone
              deriving (Show)

human = Human
ai = AI

stone (Human s) = s
stone (AI s)    = s

isHuman (Human _) = True
isHuman _         = False
isAI              = not . isHuman

isWhite (Human s) = Stone.isWhite s
isWhite (AI s)    = Stone.isWhite s
isBlack           = not . isWhite
