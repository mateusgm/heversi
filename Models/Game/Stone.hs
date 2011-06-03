{-# LANGUAGE DeriveDataTypeable #-}
module Models.Game.Stone where

import Data.Data (Data, Typeable)


data Stone = Black | White | None
              deriving (Eq, Data, Typeable)

instance Show Stone where
  show White = "x"
  show Black = "o"
  show None  = "-"

instance Read Stone where
   readsPrec _ = parsePlayer
      where parsePlayer s
              | s == "x" = (White,""):[]
              | s == "o" = (Black,""):[]
              | otherwise = (None,""):[]


white     = White
black     = Black
none      = None

isNone None   = True
isNone _      = False

isWhite White = True
isWhite _     = False

isBlack Black = True
isBlack _     = False


type Turn   = Stone
type Idle   = Stone
type Winner = Stone
type Loser  = Stone
