module Models.Game.Board    (Stone)
  where

import Data.List
import Data.Array.Diff      (array, (//), elems)
import Debug.Trace


---------- Stones

data Stone   = Black | White | Empty
               deriving (Eq)

isEmpty None = True
isEmpty _    = False

isOther White Black = True
isOther Black White = True
isOther _     _     = False


---------- Board   
  
type Position = (Int, Int)
type BoardMap = DiffArray Position Stone
data Board    = Board BoardMap | Nil

board :: Board
board         = Board $ _emptyBoard // _startingDisp

isOut :: Position -> Bool
isOut (x,y)   = x < 1 || x > 8 || y < 1 || y > 8

--- Internal

_emptyBoard :: BoardMap
_emptyBoard   = array r [((i,j),Empty) | (i,j) <- range r]
  where r     = ((1,1),(8,8))

_startingDisp :: [Move]
_startingDisp    = [((4,4),s1), ((5,5),s1), ((4,5),s2), ((5,4),s2)]
  where (s1, s2) = (Black, White)


---------- Moves

type Move = (Position, Stone)

move :: Board -> Move -> Board
move b m
  | null $ changes    = Nil
  | otherwise         = Board $ b // getChanges b m

getChanges :: Board -> Move -> [Move]
getChanges (Nil) _    = []
getChanges (Board b) (p, s)
  | isOut p           = []
  | not $ isNone b!p  = []
  | otherwise         = getChangesM b p s


--- Internal 

getChangesM :: BoardMap -> Position -> Stone -> [Move]
getChangesM b (x,y) s  = findChanges dx dy
  where dx             = [1, 1, 0, -1, -1, -1, 0, 1]
        dy             = [0, 1, 1, 1, 0, -1, -1, -1] 
        findChanges []     []     = ((x,y),s) : []
        findChanges (i:is) (j:js) = findAxisChanges (x+i,y+j) (i,j) []
                                    ++ findChanges is js
        findAxisChanges (p,q) (dp,dq) result
          | isOut (p,q)     = []
          | isEmpty b!(p,q) = []
          | b!(p,q) == s    = result
          | otherwise       = findAxisChanges (p+dp,q+dq) (dp,dq) (((p,q),s):result)


---------- Instances

instance Show Stone where
  show White = "x"
  show Black = "o"
  show None  = "-"
 
instance Show Board where
  show (Nil)     = "nil"
  show (Board b) = intersperse ' ' $ concatMap show $ makeRows $ elems b
    where makeRows l@(h:hs)    = (take 8 l) : (makeRows $ drop 8 l)
          makeRows []          = []

