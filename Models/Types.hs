{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Models.Types where

import Data.Map            (Map)
import System.State        (Component(..), End, Version, Query,
                            Update, deriveSerialize, mkMethods,
                            Data, Typeable)


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

type Board     = Map Position Player


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
            deriving (Typeable, Data, Show)

instance Version User
$(deriveSerialize ''User)

-- ==================   the Game    ================== --

data Game = Game GameState User User Int
            deriving (Typeable, Data, Show)

instance Version Game
$(deriveSerialize ''Game)

