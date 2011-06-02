module Models.Game
   (module Models.Types,
    module Models.Game
   )where

import System.State
import System.Templates

import Models.Types
import Models.Game.Engine
import Models.Repo.Game


create :: User -> User -> ServerPart Game
create u1 u2 = do game <- update $ AddGame start u1 u2
                  return game

getGame :: Int -> ServerPart Game
getGame id = do game <- query $ GetGame id
                return game
 
board :: Game -> [(Position, Player)]
board = toList . sBoard . gState

apply :: Game -> User -> Game
apply g@(Game s u1 u2 u3 id) user
   | user == u1 = g 
   | otherwise  = Game s u2 u1 u3 id

stone :: User -> Game -> Player
stone user game
   | user == gOwner game = mkBlack
   | otherwise = mkWhite

available :: Game -> [Position] 
available g = prospects (sBoard . gState $ g) . stone (gTurn g) $ g


-- template instances

instance Infoable Game where
   toMap g = insert "opponent" (show . gIdle $ g)
           . insert "stone" (show . stone (gTurn g) $ g)
           . singleton "id" . show $ gID g

instance Infoable GameState where
   toMap s = insert "turn" (show . sTurn $ s)
           . insert "white" (show . count (sBoard s) $ mkWhite)
           . singleton "black" . show . count (sBoard s) $ mkBlack

instance Infoable Position where
   toMap (x,y) = insert "x" (show x)
                 . singleton "y" $ show y

instance Infoable (Position, Player) where
   toMap (ps,pl) = insert "player" (show pl)
                 . toMap $ ps

