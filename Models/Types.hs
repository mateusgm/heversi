{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Models.Types 
   (module Data.Map,
    module Happstack.State,
    module Models.Types
   )where 

import System.Templates
import Data.Data           (Data, Typeable)
import Data.Map            (Map)
import Happstack.State     (Component(..), End, Version, Query,
                            Update, deriveSerialize, mkMethods)



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

data GameState = Play { sBoard :: Board,
                        sTurn  :: Turn,
                        sIdle  :: Idle }  |
                 Over { sBoard :: Board,
                        sWinner :: Winner,
                        sLoser :: Loser } |
                 Draw { sBoard :: Board }
                 deriving (Show, Data, Typeable)

instance Version GameState
$(deriveSerialize ''GameState)


-- ==================   the User    ================== --

data User = Android |
            Human { uName :: String,
                    uID :: Int } 
            deriving (Eq, Typeable, Data, Show)

instance Version User
$(deriveSerialize ''User)

-- ==================   the Game    ================== --

data Game = Game { gState :: GameState,
                   gTurn :: User,
                   gIdle :: User,
                   gOwner :: User,
                   gID :: Int }
            deriving (Typeable, Data, Show)

instance Version Game
$(deriveSerialize ''Game)

