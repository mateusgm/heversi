{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Models.Game
   (module Models.Game.Engine,
    module Models.Game.Board,
    module Models.Game.Stone,
    module Models.Game
   )where

import Data.Data           (Data, Typeable)
import System.Templates

import Models.Game.Engine
import Models.Game.Board
import Models.Game.Stone
import Models.Game.AI
import Models.User


data Game = Game { gState :: GameState,
                   gTurn :: User,
                   gIdle :: User,
                   gID :: Int }
            deriving (Eq, Typeable, Data, Show)

create' :: User -> User -> Int -> Game
create' u1 u2 id = Game start u1 u2 id

play' :: Game -> User ->  Move -> Game
play' game@(Game state turn idle id) user m@(ps,pl)
   | user /= turn = game
   | otherwise    = Game state' turn' idle' id
   where state' = if (isHuman user) then play'' state m
                  else play'' state . move (sBoard state) $ pl
         (turn', idle') = vez game state'

vez :: Game -> GameState -> (User,User)
vez g@(Game _ t i _) state'
   | stone t g == turn state' = (t, i)
   | otherwise                = (i, t)

board :: Game -> [(Position, Stone)]
board = toList . sBoard . gState

stone :: User -> Game -> Stone
stone user game
   | user == gTurn game = turn . gState $ game
   | otherwise          = idle . gState $ game

available :: Game -> [Position] 
available g = prospects (sBoard . gState $ g) . stone (gTurn g) $ g

checkUser :: User -> Game -> Bool
checkUser u (Game _ u1 u2 _)
   | u == u2   = True
   | u == u1   = True
   | otherwise = False


-- template instances

instance Infoable Game where
   toMap g = insert "turn" (show . uID . gTurn $ g)
           . insert "idle" (show . uID . gIdle $ g)
           . singleton "id" . show $ gID g

instance Infoable GameState where
   toMap s = insert "turn" (show . turn $ s)
           . insert "idle" (show . idle $ s)
           . insert "status" (status s)
           . insert "white" (show . count (sBoard s) $ white)
           . singleton "black" . show . count (sBoard s) $ black

instance Infoable Position where
   toMap (y,x) = singleton "id" $ (show y) ++ (show x)

instance Infoable (Position, Stone) where
   toMap ((y,x),pl) = insert "player" (show pl)
                    . insert "y" (show y)
                    . singleton "x" $ show x

