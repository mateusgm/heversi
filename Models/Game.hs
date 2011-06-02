module Models.Game
   (module Models.Types,
    module Models.Game
   )where

import System.State
import System.Templates

import Models.Types
import qualified Models.Game.Engine as Engine (play)
import Models.Game.Engine hiding (play)
import Models.Repo.Game
import Control.Monad.Trans


create :: User -> User -> ServerPart Game
create u1 u2 = do game <- update $ AddGame start u1 u2
                  return game

getGame :: Int -> ServerPart Game
getGame id = do game <- query $ GetGame id
                return game
 
play :: Game -> User ->  Move -> ServerPart Game
play game@(Game state turn idle owner id) user m
   | user /= turn = return game
   | otherwise    = do let state' = Engine.play state m 
                           game' = Game state' idle turn owner id
                       update $ SaveGame game'
                       return game'

board :: Game -> [(Position, Player)]
board = toList . sBoard . gState

apply :: Game -> User -> Game
apply g@(Game s t i o id) user
   | user == t  = g
   | otherwise  = Game s i t o id

player :: User -> Game -> Player
player user game
   | user == gOwner game = mkBlack
   | otherwise           = mkWhite

available :: Game -> [Position] 
available g = prospects (sBoard . gState $ g) . player (gTurn g) $ g


-- template instances

instance Infoable Game where
   toMap g = insert "opponent" (show . uID . gIdle $ g)
           . insert "stone" (show . player (gTurn g) $ g)
           . singleton "id" . show $ gID g

instance Infoable GameState where
   toMap s = insert "turn" (show . sTurn $ s)
           . insert "white" (show . count (sBoard s) $ mkWhite)
           . singleton "black" . show . count (sBoard s) $ mkBlack

instance Infoable Position where
   toMap (y,x) = insert "x" (show x)
                 . singleton "y" $ show y

instance Infoable (Position, Player) where
   toMap (ps,pl) = insert "player" (show pl)
                 . toMap $ ps

