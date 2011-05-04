module Models.Game.Player     (Player, Turn, Idle, Winner, Loser,
                               mkWhite, mkBlack, mkNone,
                               white, black, none) 
  where

import Models.Types           (Player(..)) 


mkWhite     = White
mkBlack     = Black
mkNone      = None

none None   = True
none _      = False

white White = True
white _     = False

black Black = True
black _     = False

