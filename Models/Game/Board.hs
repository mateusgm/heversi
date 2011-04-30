module Models.Game.Board    
  where

import Data.List
import Data.Array.Diff      (DiffArray, array, (//), (!), elems, range)
import Debug.Trace


---------- Stones

data Stone   = Black | White | None
               deriving (Eq)

isNone None = True
isNone _     = False

isOther White Black = True
isOther Black White = True
isOther _     _     = False

isWhite :: Stone -> Bool

isBlack :: Stone -> Bool

---------- Board   

type Score = (Integer, Integer)  
type Position = (Integer, Integer)
type BoardMap = DiffArray Position Stone
data Board    = Board BoardMap | Nil

board :: Board
board         = Board $ _emptyBoard // _startingDisp

isOut :: Position -> Bool
isOut (x,y)   = x < 1 || x > 8 || y < 1 || y > 8

isNil :: Board -> Bool
isNil Nil = True
isNil _   = False

score :: Board -> Score
score (Board b) = foldr countStone (0,0) $ elems b
  where countStone s (w,b)
          | iswhite s = (w+1,b)
          | isBlack s = (w,b+1)
          | otherwise = (w,b)
  
--- Internal

_emptyBoard :: BoardMap
_emptyBoard   = array r [((i,j),None) | (i,j) <- range r]
  where r     = ((1,1),(8,8))

_startingDisp :: [Move]
_startingDisp    = [((4,4),s1), ((5,5),s1), ((4,5),s2), ((5,4),s2)]
  where (s1, s2) = (Black, White)


---------- Moves

type Move      = (Position, Stone)
type Prospects = [Move]

move :: Board -> Move -> Board
move b@(Board bm) m
  | null $ changes    = Nil
  | otherwise         = Board $ bm // changes
  where changes = getChanges b m

getChanges :: Board -> Move -> [Move]
getChanges (Nil) _     = []
getChanges (Board b) (p, s)
  | isOut p            = []
  | not $ isNone $ b!p = []
  | otherwise          = getChangesM b p s

getMoves :: Board -> [Move]

getProspects :: Board -> Stone -> Prospects

--- Internal 

getChangesM :: BoardMap -> Position -> Stone -> [Move]
getChangesM b (x,y) s
  | null changes       = []
  | otherwise          = ((x,y),s) : changes                
  where changes        = findChanges dx dy
        dx             = [1, 1, 0, -1, -1, -1, 0, 1]
        dy             = [0, 1, 1, 1, 0, -1, -1, -1] 
        findChanges []     _      = []
        findChanges (i:is) (j:js) = findChanges is js  ++
                                     lookChanges (x+i,y+j) (i,j) []
        lookChanges (p,q) (dp,dq) result
          | isOut (p,q)      = []
          | isNone $ b!(p,q) = []
          | b!(p,q) == s     = result
          | otherwise        = lookChanges (p+dp,q+dq) (dp,dq)
                                (((p,q),s):result)


---------- Instances

instance Show Stone where
  show White = "x"
  show Black = "o"
  show None  = "-"
 
instance Show Board where
  show (Nil)     = "nil"
  show (Board b) = intersperse ' ' $ concatMap ((:) '\n' . concat)
                    $ makeRows $ map show $ elems b

makeRows l@(h:hs)    = (take 8 l) : (makeRows $ drop 8 l)
makeRows []          = []

