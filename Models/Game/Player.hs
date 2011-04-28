module Models.Game.Player where

----- PlayerType

data PlayerType = Human | IA
                  
human           = Human
ia              = IA                  

isHuman (Human) = True
isHuman _       = False

isIA            = not . isHuman

----- Player

data Player     = White PlayerType
                  Black PlayerType
                  None
                  deriving (Eq)

instance Show Player where
  show (White _) = "x"
  show (Black _) = "o"
  show (None)    = "-"
   
players t       = (White human, Black t)

isOther                   :: Player -> Player -> Bool
other (White _) (Black _) = True
other (Black _) (White _) = False



human
