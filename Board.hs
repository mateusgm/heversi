module Board
where

import Data.List
import Data.Array.Diff
import Debug.Trace

--------

data Player     = Player PlayerID PlayerType | None deriving (Eq)
type PlayerID   = Int
data PlayerType = Human | IA deriving (Eq, Show)

other           :: Player -> Player -> Bool
other a b       = a /= None && b /= None && a /= b


instance Show Player where
   show (Player id t) = if (id == 1) then "x" else "o"
   show (None)        = "-"

--------

type Board    = DiffArray Position Player
type Position = (Int, Int)

board         :: PlayerType -> Board
board pt      = array r [((i,j),None) | (i,j) <- range r] // 
                [((4,4),p1), ((5,5),p1), ((4,5),p2), ((5,4),p2)]
                where
                r  = ((1,1),(8,8))
                p1 = Player 1 Human
                p2 = Player 2 pt

outBounds       :: Position -> Bool
outBounds (x,y) = x < 1 || x > 8 || y < 1 || y > 8

nullBoard     = array ((0,0),(0,0)) [((0,0),None)]

data ShowBoard = ShowBoard Board
instance Show ShowBoard where
   show (ShowBoard b)   = trace result ""
      where
      result               =  intersperse ' '
                              . concatMap ((:) '\n' . map toChar)
                              . makeRows $ elems b
      makeRows l@(h:hs)    = (take 8 l) : (makeRows $ drop 8 l)
      makeRows []          = []
      toChar (Player id t) = if (id == 1) then 'x' else 'o'
      toChar None          = '-'

sb b  = show (ShowBoard b)
sbc b = trace (show (ShowBoard b)) b

--------

data Move = Move Player Position deriving (Show)

move      :: Board -> Move -> Board
move b (Move pl (x,y))
   | b!(x,y) /= None = nullBoard
   | null changes    = nullBoard
   | otherwise       = b // changes
   where 
   dx                        = [1, 1, 0, -1, -1, -1, 0, 1]
   dy                        = [0, 1, 1, 1, 0, -1, -1, -1] 
   changes                   = findChanges dx dy        
   findChanges (i:is) (j:js) = findAxisChanges (x+i,y+j) (i,j) [] ++ findChanges is js
   findChanges []     []     = ((x,y),pl) : []
   findAxisChanges (p,q) (dp,dq) result
      | outBounds (p,q)  = []
      | b!(p,q) == None  = []
      | b!(p,q) == pl    = result
      | otherwise        = findAxisChanges (p+dp,q+dq) (dp,dq) (((p,q),pl):result)         




--------- teste

p = Player 1 Human
b = board Human
m = Move p (6,4)


