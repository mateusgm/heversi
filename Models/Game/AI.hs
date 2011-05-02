module Models.Game.AI     (move)
  where

import Models.Game.Board  (Board, Move, prospects)  
import Models.Game.Stone  (Stone)

move :: Board -> Stone -> Move
move b s = (head $ prospects b s, s)


