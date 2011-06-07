module Adaptors.Game
   (module Models.Game,
    module Adaptors.Game
   )where

import System.State

import Models.Repo.Game
import Models.Game
import Models.User


create :: User -> User -> ServerPart Game
create u1 u2 = do game <- update $ AddGame u1 u2
                  return game

get :: Int -> ServerPart Game
get id = do game <- query $ GetGame id
            return game
 
check :: User -> ServerPart (Maybe Game)
check u = do game <- query $ GetUserGame u
             return game
 
play :: Game -> User ->  Move -> ServerPart Game
play game user move
   | game == game' = return game
   | otherwise     = do update $ SaveGame game'
                        return game'
   where game' = play' game user move

