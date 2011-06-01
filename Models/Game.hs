module Models.Game
   (module Models.Types,
    module Models.Game
   )where

import System.State
import System.Templates

import Models.Types
import Models.Game.Engine
import Models.Repo.Game

import Prelude hiding (id)

begin :: User -> User -> ServerPart Game
begin u1 u2 = do game <- update $ AddGame start u1 u2
                 return game

getGame :: Int -> ServerPart Game
getGame id = do game <- query $ GetGame id
                return game
 
board (Game g u1 u2 i) = u1
state = board 

instance Infoable Game where
   toMap (Game g u1 u2 i) = insert "u1" (show . uID $ u1)
                          . insert "u1" (show . uID $ u2)
                          . singleton "id" $ show i









