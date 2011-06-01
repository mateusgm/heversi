module Models.Game
   (module Models.Types,
    module Models.Game
   )where

import System.State
import System.Templates

import Models.Types
import Models.Game.Engine
import Models.Repo.Game

begin :: User -> User -> ServerPart Game
begin u1 u2 = do game <- update $ AddGame start u1 u2
                 return game

getGame :: Int -> ServerPart Game
getGame id = do game <- query $ GetGame id
                return game
 
board = toList . sBoard . gState

instance Infoable Game where
   toMap g = insert "black" (show . gBlack $ g)
           . insert "white" (show . gWhite $ g)
           . singleton "id" . show $ gID g

instance Infoable GameState where
   toMap s = insert "turn" (show . sTurn $ s)
           . singleton "idle" . show $ sIdle s

instance Infoable (Position, Player) where
   toMap (ps,pl) = insert "position" position
                 .  singleton "player" $ show pl 
      where position = show (fst ps) ++ show (snd ps)







