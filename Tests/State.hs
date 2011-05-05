module Tests.State          ()
  where


import System.State         (state)
import Happstack.State      (query, update)

import Models.Game.Engine
import Models.Types
import Adaptors.Game

main = do state
          id1 <- update . AddGame . Game start Android $ Android
          print id1
          g1 <- query . GetGame $ id1 
          print g1
