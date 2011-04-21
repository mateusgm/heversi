import Data.Array
import Data.List
import Data.Maybe
import System.IO

type Index = (Int, Int)
type Player = Int -- (-1, 0, 1) for white, unclaimed and black, resp.
type Board = Array Index Player

-- Infix function which add two vectors
(|+|) :: Index -> Index -> Index
(i, j) |+| (k, l) = (i + k, j + l)

-- The functions dim, pieces and board2Str must be updated if one wishes to
-- alter the board's dimensions.

-- Board dimensions
dim :: (Index, Index)
dim = ((0, 0), (7, 7))

-- The pieces that will initially be on the board
pieces :: [(Index, Player)]
pieces = zip (range ((3, 3), (4, 4))) [1, -1, -1, 1]

-- Return a textual representation of the given board
board2Str :: Board -> String
board2Str b = ' ' : ['a'..'h'] ++                               -- column names
  concatMap (\i -> '\n' : show (i + 1) ++                       -- row names
    map player2Str (take 8 $ drop (i * 8) $ elems b)) [0..7] ++ "\n"  -- rows

-- The initial board, including the initial pieces
initial :: Board
initial = array dim (zip (range dim) $ repeat 0) // pieces -- the empty board

-- The directions in which pieces can be flipped.
deltas :: [(Int, Int)]
deltas = tail [(i, j) | i <- [0,1,-1], j <- [0,1,-1]]

-- All 'lines' in any direction starting from the given index
dirs :: Index -> [[Index]]
dirs c = map (takeWhile (inRange dim) . ($ c) . iterate . (|+|)) deltas

-- Attempt to perform a move
move :: Board -> Player -> Index -> Maybe Board
move b p i | b ! i /= 0 = Nothing
           | otherwise  = if b == updateAll then Nothing else Just updateAll
  where
    -- Attempt to swap pieces in all directions
    updateAll = foldr update b (dirs i)
    -- Attempt to swap pieces in the given direction
    update r b' = b' // case (group $ map (b' !) r) of
      ([_]:c@(m:_):(l:_):_) | m /= 0 && l == p ->     -- all conditions are met
        zip (take (1 + length c) $ r) (repeat p)      -- so swap the pieces
      _                                        -> []  -- nothing to swap

player2Str :: Player -> Char
player2Str = fromJust . (`lookup` zip [-1..1] "b w")

-- Test whether the given player can make a move
possible :: Board -> Player -> Bool
possible b p = mapMaybe (move b p) (range dim) /= []

-- Ask the players to make a move in turns, and update the board when required
mainLoop :: Board -> Player -> IO ()
mainLoop b p = do
  putStr $ player2Str p : "> "
  hFlush stdout
  c:r <- getLine  -- get user input (this is very fragile!)
  -- Attempt to perform the suggested move...
  case move b p (read r - 1, head $ elemIndices c ['a'..]) of
    Nothing -> mainLoop b p  -- ...try again if not possible
    Just b' -> do            -- ...accept if possible
      putStr $ board2Str b'  -- print the new board state
      case filter (possible b') [-1 * p, p] of  -- select next player to move
        (p':_) -> mainLoop b' p'   -- move if possible, otherwise game over
        _      -> print $ if winner == ' ' then 'd' else winner
                    where winner = player2Str $ signum $ sum $ elems b'

-- Let the games begin!
main :: IO()
main = putStr (board2Str initial) >> mainLoop initial 1
