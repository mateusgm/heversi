module Models.Game.Board  (Score, Position, Move, Prospects, Board(..),
                           change, board, prospects, changes, score, isOut)
  where

import Data.List
import Data.Array.Diff    (DiffArray, array, (//), (!), elems, range)
import Models.Game.Stone  (Stone, black, white, none,
                           isNone, isWhite, isBlack)


-- some definitions

type Score     = (Integer,  Integer)  
type Position  = (Integer,  Integer)
type Move      = (Position, Stone)
type Prospects = [Position]
type BoardMap  = DiffArray Position Stone

-- the Board abstract datatype

newtype Board  = Board BoardMap 

instance Show Board where
  show (Board b) = intersperse ' ' $ (:) '\n' $ unlines $ map concat
                   $ rows $ map show $ elems b
    where rows l@(h:hs) = (take 8 l) : (rows $ drop 8 l)
          rows []       = []

-- apply changes in the board  

change :: Board -> [Move] -> Board
change (Board b) cs = Board $ b // cs

-- get a board

board :: Board
board = Board $ _emptyBoard // _initChangs

_range      = ((1,1),(8,8))
_boardRange = range _range
_emptyBoard = array _range [((i,j),none) | (i,j) <- _boardRange]
_initChangs = [((4,4),black), ((4,5),white),
               ((5,4),white), ((5,5),black)]

-- get changes that a move may arise in a board  

prospects :: Board -> Stone -> Prospects
prospects b s = [p | p <- _boardRange,
                 not $ null $ changes b (p,s)]

-- get changes that a move may arise in a board

changes :: Board -> Move -> [Move]
changes (Board b) m@(p,s)
  | isOut p            = []
  | not $ isNone $ b!p = []
  | null cs            = []
  | otherwise          = m:cs
  where cs = changesM b m
  
-- get changes that a move may arise in a BoardMap

changesM :: BoardMap -> Move -> [Move]
changesM b ((x,y),s) = findChanges dx dy
  where dx           = [1, 1, 0, -1, -1, -1, 0, 1]
        dy           = [0, 1, 1, 1, 0, -1, -1, -1] 
        findChanges []     _      = []
        findChanges (i:is) (j:js) = findChanges is js  ++
                                     lookChanges (x+i,y+j) (i,j) []
        lookChanges (p,q) (dp,dq) result
          | isOut (p,q)      = []
          | isNone $ b!(p,q) = []
          | b!(p,q) == s     = result
          | otherwise        = lookChanges (p+dp,q+dq) (dp,dq)
                                (((p,q),s):result)  

-- get the score of whites and blacks in a board

score :: Board -> Score
score (Board b) = foldr countStone (0,0) $ elems b
  where countStone s (w,b)
          | isWhite s = (w+1,b)
          | isBlack s = (w,b+1)
          | otherwise = (w,b)

-- check if position is out of range

isOut :: Position -> Bool
isOut (x,y)   = x < a || y < b || x > c || y > d
  where ((a,b), (c,d)) = _range

