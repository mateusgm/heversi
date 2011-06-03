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

instance Read Player where
   readsPrec _ = parsePlayer
      where parsePlayer s
              | s == "x" = (White,""):[]
              | s == "o" = (Black,""):[]
              | otherwise = (None,""):[]

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
                 deriving (Eq, Show, Data, Typeable)

instance Version GameState
$(deriveSerialize ''GameState)


-- ==================   the User    ================== --

data User = Android |
            Human String Int 
            deriving (Eq, Typeable, Data, Show)

isHuman Android = False
isHuman (Human _ _) = True

uID Android = 0
uID (Human _ i) = i

instance Version User
$(deriveSerialize ''User)

-- ==================   the Game    ================== --

data Game = Game { gState :: GameState,
                   gTurn :: User,
                   gIdle :: User,
                   gOwner :: User,
                   gID :: Int }
            deriving (Eq, Typeable, Data, Show)

instance Version Game
$(deriveSerialize ''Game)

