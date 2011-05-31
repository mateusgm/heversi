module Models.Game 
  where

import qualified Adaptors.Game as G  
import Models.Game.Engine     (play, start)
import Models.Types           (User, Game(..))

begin :: User -> User -> ServerPart Game
begin u1 u2 = do game <- update $ G.AddGame u1 u2
                 return game

getGame :: Int -> ServerPart Game
getGame id = do game <- query $ G.GetGame id
                return game
 
               









