module Models.Game.Board    (Position, Move, Flip, Board, board, 
                             count, flip, flips, prospects, out)
  where

import Debug.Trace
import Prelude               hiding (flip, filter)
import Data.Array           (range)
import Data.Map             (update, fromList, (!), size, 
                             findMin, findMax, keys, filter)

import Models.Types         (Board, Move, Flip, Position, Direction) 
import Models.Game.Player   (Player, black, white, none,
                             mkWhite, mkBlack, mkNone)


-- =============== Low Level Operations =============== --

-- constants
_bounds = ((1,1),(8,8))
_range  = range _bounds
_empty  = [((i,j),mkNone) | (i,j) <- _range]
_flips  = [((4,4),b),((4,5),w),((5,4),w), ((5,5),b)]
  where (w,b) = (mkWhite, mkBlack)

-- update a given position of a board with a new stone
update' :: Move -> Board -> Board
update' (p,s) b = update (choose s) p b
  where choose s' _ = Just s'

-- update the board according to the given moves
updates :: Board -> [Move] ->Board
updates b ms = foldr update' b ms

-- get the initial board
board :: Board
board = updates (fromList _empty) _flips

-- get the player that owns a given position on the board
player :: Board -> Position -> Player
player b p = b!p

-- get the score of a given player on the board
count :: Board -> Player -> Int
count b s = size . filter (== s) $ b

-- check if a given position on the board is empty
empty :: Board -> Position -> Bool
empty b = none . player b

-- check if position is out of the range of the board
out :: Board -> Position -> Bool
out b (x,y) = x < m || x > p || y < n || y > q     
  where (m,n) = fst . findMin $ b
        (p,q) = fst . findMax $ b

flip :: Board -> [Flip] -> Board
flip = updates

-- check if a move in the given position is valid on the board
valid :: Board -> Position -> Bool
valid b p = (not . out b $ p) && (empty b $ p)

-- get state of the board in a given a direction,
-- beginning from a give position
direction :: Board -> Direction -> Position -> [Move]
direction b (di,dj) (i,j)
  | not . out b $ (i,j) = current : direction b (di,dj) (i+di,j+dj) 
  | otherwise           = []
  where current = ((i,j), player b (i,j))


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

-- get the possible moves for the given player on the board   
prospects :: Board -> Player -> [Position]
prospects b s = [ p | p <- keys b,
                  not . null . flips b $ (p,s) ]

