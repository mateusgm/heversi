module Models.Game.AI (
      move
   ) where

import Models.Game.Board 
import Models.Game.Stone

move :: Board -> Stone -> Move
move b s = (head $ prospects b s, s)


