module Models.Game.Stone (Stone(..),black, white, none,
                          isNone, isWhite, isBlack)
  where  


data Stone   = Black | White | None
               deriving (Eq)

black = Black
white = White
none  = None

isNone None   = True
isNone _      = False

isWhite White = True
isWhite _     = False

isBlack Black = True
isBlack _     = False

isOther White Black = True
isOther Black White = True
isOther _     _     = False


instance Show Stone where
  show White = "x"
  show Black = "o"
  show None  = "-"
