module Models.Game.Player    (Player, mkWhite, mkBlack, flip,
                              black, white, none, other)
  where


data Player  = Black | White | None
               deriving (Eq)

instance Show Player where
  show White = "x"
  show Black = "o"
  show None  = "-"


mkWhite     = White
mkBlack     = Black

flip White  = Black
flip Black  = White
flip _      = None

none None   = True
none _      = False

white White = True
white _     = False

black Black = True
black _     = False

other White Black = True
other Black White = True
other _     _     = False

