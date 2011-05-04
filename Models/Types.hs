{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Models.Types
  where

import Data.Data              (Data, Typeable)
import Happstack.State        (Component(..), End, Version,
                               deriveSerialize)
--import Data.Array.Diff        (DiffArray, elems)
import Data.Array             (Array, elems)
import Data.Map               (Map, empty)
import Data.List              (intersperse)


-- ==================   the Player  ================== --

data Player = Black | White | None
               deriving (Eq, Data, Typeable)

instance Show Player where
  show White = "x"
  show Black = "o"
  show None  = "-"

instance Version Player
$(deriveSerialize ''Player)

type Turn   = Player
type Idle   = Player
type Winner = Player
type Loser  = Player


-- ==================   the Board   ================== --

type Position  = (Integer,  Integer)
type Direction = (Integer,  Integer)
type Move      = (Position, Player)
type Flip      = Move

type BoardMap  = Array Position Player

newtype Board  = Board BoardMap
                 deriving (Data, Typeable)

instance Show Board where
  show (Board b) = intersperse ' ' . (:) '\n' . unlines . map concat
                   . rows . map show . elems $ b
    where rows l@(h:hs) = (take 8 l) : (rows $ drop 8 l)
          rows []       = []

instance Version Board
$(deriveSerialize ''Board)


-- ================== the GameState ================== --

data GameState = Play Board Turn   Idle  |
                 Over Board Winner Loser |
                 Draw Board
                 deriving (Show, Data, Typeable)

instance Version GameState
$(deriveSerialize ''GameState)


-- ==================   the User    ================== --

data User = Android |
            Human String Int 
            deriving (Typeable, Data)

instance Version User
$(deriveSerialize ''User)


-- ==================   the Game    ================== --

data Game = Game GameState User User
            deriving (Typeable, Data)

instance Version Game
$(deriveSerialize ''Game)


-- ==================  the GameDir  ================== --

newtype GameDir = GameDir (Map Int Game)
                  deriving (Typeable, Data)

instance Version GameDir
$(deriveSerialize ''GameDir)

instance Component GameDir where
  type Dependencies GameDir = End
  initialValue = GameDir empty  

