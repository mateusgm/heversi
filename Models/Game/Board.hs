module Models.Game.Board  (Score, Position, Move, Prospects, Board,
                           change, board, prospects, changes, score)
  where

import Data.List
import Data.Array.Diff    (DiffArray, array, (//), (!), elems, range)
import Models.Game.Stone  (Stone, black, white, none,
                           isNone, isWhite, isBlack)

-- some definitions
type Position  = (Integer,  Integer)
type Direction = (Integer,  Integer)
type Move      = (Position, Player)
type BoardMap  = DiffArray Position Player

-- the Board abstract datatype
newtype Board  = Board BoardMap 
instance Show Board where
  show (Board b) = intersperse ' ' . (:) '\n' . unlines . map concat
                   . rows . map show . elems $ b
    where rows l@(h:hs) = (take 8 l) : (rows $ drop 8 l)
          rows []       = []

-- constants
_range      = ((1,1),(8,8))
_boardRange = range _range
_emptyBoard = array _range [((i,j),none) | (i,j) <- _boardRange]
_initChangs = [((4,4),black), ((4,5),white),
               ((5,4),white), ((5,5),black)]

-- get a board
board :: Board
board = Board $ _emptyBoard // _initChangs

-- get the player that owns a position at the board
player :: Board -> Position -> Player
player (Board b) p = b!p

-- flip stones that are in certain positions  
flip :: Board -> [Position] -> Board
flip (Board b) ps = Board $ b // [(p,Player.flip s)| (p,s) <- ps] 

-- get flips positions that a move may arise in a board
flips :: Board -> Move -> [Position]
flips b m = concatMap (flipsDir b m) dirs
  where dirs = [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]

-- get flips positions that a move may arise in a board direction
flipsDir :: Board -> Move -> Direction -> [Position]
flipsDir b (p,s) d 
  | valid p     = []
  | otherwise   = affectedBy p'
  where valid p'        = (empty b $ p') && (not . null . affectedBy $ p')
        affectedBy      = affected [] direction'
        direction'      = tail . direction b d
        affected a []   = []
        affected a (p',s'):xs 
          | s' == none  = []
          | s' == s     = if (null a) then [] else p:a
          | otherwise   = affected (p':a) xs 

-- get state of the board in a given a direction,
-- beginning from a give position
direction :: Board -> Direction -> Position -> [(Position, Player)]
direction b (di,dj) (i,j)
  | not . out b $ (i,j) = current : direction b (di,dj) (i+di,j+dj) 
  | otherwise           = []
  where current = ((i,j), player b (i,j))

-- get the possible moves for the given a player   
prospects :: Board -> Player -> [Position]
prospects b s = [ p | p <- range . bound $ b,
                  not . null . flips b $ (p,s) ]

-- get the score of a given player in a board
count :: Board -> Player -> Int
count b s = length . filter ((==) s) . elems $ b

-- check if a given position in the board is empty
empty :: Board -> Position -> Bool
empty = none . player

-- check if position is out of range
out :: Board -> Position -> Bool
out b (x,y) = x < a || y < b || x > c || y > d
  where ((a,b), (c,d)) = range . bound $ b

