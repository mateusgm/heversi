module Models.Game.Board    (Position, Move, Flip, Board, board, 
                             count, flip, flips, prospects)
  where

import Debug.Trace
import Prelude               hiding (flip)
import Data.List            (filter, length, intersperse)
import Data.Array.Diff      (DiffArray, array, (//), (!), elems,
                             bounds, range)
import qualified
  Models.Game.Player as P   (flip)
import Models.Game.Player   (Player, black, white, none, other,
                             mkWhite, mkBlack, mkNone)


-- =============== The Board datatype =============== --

-- some definitions
type Position  = (Integer,  Integer)
type Direction = (Integer,  Integer)
type Move      = (Position, Player)
type Flip      = Move

type BoardMap  = DiffArray Position Player

-- the abstract type
newtype Board  = Board BoardMap 
instance Show Board where
  show (Board b) = intersperse ' ' . (:) '\n' . unlines . map concat
                   . rows . map show . elems $ b
    where rows l@(h:hs) = (take 8 l) : (rows $ drop 8 l)
          rows []       = []

-- =============== Low Level Operations =============== --

-- constants
_bounds = ((1,1),(8,8))
_range  = range _bounds
_empty  = array _bounds [((i,j),mkNone) | (i,j) <- _range]
_flips  = [((4,4),b),((4,5),w),((5,4),w), ((5,5),b)]
  where (w,b) = (mkWhite, mkBlack)

-- get a board
board :: Board
board = Board $ _empty // _flips

-- get the player that owns a given position on the board
player :: Board -> Position -> Player
player (Board b) p = b!p

-- get the score of a given player on the board
count :: Board -> Player -> Int
count (Board b) s = length . filter ((==) s) . elems $ b

-- check if a given position on the board is empty
empty :: Board -> Position -> Bool
empty b = none . player b

-- check if position is out of the range of the board
out :: Board -> Position -> Bool
out (Board b) (x,y) = x < x' || y < y' || x > x'' || y > y''
  where ((x',y'),(x'',y'')) = bounds b

-- flip stones that are in certain positions of the board 
flip :: Board -> [Flip] -> Board
flip (Board b) ms = Board $ b // ms

-- check if a move in a given position is valid on the board
valid :: Board -> Position -> Bool
valid b p = (not . out b $ p) && (empty b $ p)

-- =============== High Level operations =============== --

-- get flips that a move may arise on the board
flips :: Board -> Move -> [Flip]
flips b m@(p,s)
  | valid b p = concatMap (flipsDir b m) dirs
  | otherwise = []
  where dirs  = [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]

-- get flips positions that a move may arise on the board direction
flipsDir :: Board -> Move -> Direction -> [Flip]
flipsDir b (p,s) d 
  | validDir    = affectedBy p
  | otherwise   = []
  where validDir        = not . null . affectedBy $ p
        affectedBy      = affected [] . direction'
        direction'      = tail . direction b d
        affected a []   = []
        affected a (x:xs)
          | none s'     = []
          | s == s'     = if (null a) then [] else (p,s):a
          | otherwise   = affected ((p',s):a) xs 
          where (p',s') = x

-- get state of the board in a given a direction,
-- beginning from a give position
direction :: Board -> Direction -> Position -> [(Position, Player)]
direction b (di,dj) (i,j)
  | not . out b $ (i,j) = current : direction b (di,dj) (i+di,j+dj) 
  | otherwise           = []
  where current = ((i,j), player b (i,j))

-- get the possible moves for the given a player   
prospects :: Board -> Player -> [Position]
prospects b@(Board bm) s = [ p | p <- range . bounds $ bm,
                          not . null . flips b $ (p,s) ]

