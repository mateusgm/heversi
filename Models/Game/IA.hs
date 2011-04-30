module Models.Game.IA where

import Models.Game.Board

getMove :: Board -> Stone -> Move
getMove b s = (head prospects, s)
  where prospects = getProspects b s

