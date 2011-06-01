module Models.Game where

import Models.Type
import Models.Repo.Game
import System.State


begin :: User -> User -> ServerPart Game
begin u1 u2 = do game <- update $ G.AddGame u1 u2
                 return game

getGame :: Int -> ServerPart Game
getGame id = do game <- query $ G.GetGame id
                return game
 
               









