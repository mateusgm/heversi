module Models.Game.AI     (move)
  where

import Models.Game.Board   (Board, Move, prospects)  
import Models.Game.Player  (Player)

move :: Board -> Player -> Move
move b s = (head $ prospects b s, s)


