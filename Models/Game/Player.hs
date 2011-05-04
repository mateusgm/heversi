module Models.Game.Player    (Player, Turn, Idle, Winner, Loser,
                              mkWhite, mkBlack, mkNone,
                              black, white, none, other, flip)
  where

import Prelude                hiding (flip)

type Turn   = Player
type Idle   = Player
type Winner = Player
type Loser  = Player

data Player = Black | White | None
               deriving (Eq)

instance Show Player where
  show White = "x"
  show Black = "o"
  show None  = "-"


mkWhite     = White
mkBlack     = Black
mkNone      = None

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

