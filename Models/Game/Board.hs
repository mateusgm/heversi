module Models.Game.Board where

import Data.List
import Data.Array.Diff      (array, (//), elems)
import Debug.Trace
import Models.Game.Player   (players, Player)


type Position = (Int, Int)
type BoardMap = DiffArray Position Player

data Board    = Board BoardMap | Nil

nullBoard = Nil                

board   :: PlayerType -> Board
board t =  Board $ emptyBoard // startDisp
  where (p1, p2)    = players t
        startDisp   = [((4,4),p1), ((5,5),p1), ((4,5),p2), ((5,4),p2)]

emptyBoard = array r [((i,j),None) | (i,j) <- range r]
  where r  = ((1,1),(8,8))

isOut       :: Position -> Bool
isOut (x,y) = x < 1 || x > 8 || y < 1 || y > 8

-- show Board

instance Show Board where
  show (Nil)     = "nil"
  show (Board b) = intersperse ' ' $ concatMap show $ makeRows $ elems b
    where makeRows l@(h:hs)    = (take 8 l) : (makeRows $ drop 8 l)
          makeRows []          = []

