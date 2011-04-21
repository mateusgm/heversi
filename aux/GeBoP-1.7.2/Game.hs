{-# OPTIONS -fglasgow-exts #-}

module Game where

import Graphics.UI.WX hiding (children, value)
import Array
import Tools

----------------
-- class Game --
----------------

type Player = Int
type Move g = (Player, g) -> (Player, g)
type Value  = [Float]

data Properties
  = Properties { players   :: Int
               , boardsize :: Int
               , human     :: [Bool]
               }
  deriving Show

data PropertyRange
  = PropertyRange { playersrange   :: [Int]
                  , boardsizerange :: [Int]
                  }

class (Eq g, Show g) => Game g where

  name        :: g -> String
--{  rules       :: g -> String
--{  information :: g -> String

  standard    :: g -> Properties
  possible    :: g -> PropertyRange
  
  new         :: Properties -> g

  moves       :: Properties -> Player -> g -> [Move g]
  showmove    :: Properties -> Player -> g -> Int -> String

  value       :: Properties -> Player -> g -> Value

  board       :: Panel ()   -> Properties -> Var (Tree g) -> Bool -> (Int -> IO ()) -> IO ()

data GeneralGame = forall g. Game g => Game g

---------------
-- data Tree --
---------------

data Game g => Tree g
  = Node { player   :: Player
         , state    :: g
         , movesnr  :: Int
         , childid  :: Int
         , mov      :: Array Int (Move g) --{ onnodig?
         , children :: Array Int (Tree g)
         , val      :: Value -- should be equally good for the current player
                               --{ misschien handig om een set te nemen ipv list?
                               --{ maar: voor gemiddeldes juist weer niet! (ook in algoritme?)
                               --{ in dat geval: maar een entry per zet! (dus length best = length val)
                               --{ > nee! we doen gewoon 1 value, het gemiddelde van alle bests
                               --{ val = average $ map val best
         , best     :: [Int]
         , filled   :: Bool
         , closed   :: Bool
         , mind     :: Inf Int
         , maxd     :: Int
         , volume   :: Int
         }

buildtree :: Game g => Properties -> Player -> g -> Tree g
buildtree prop p g = let ms = moves prop p g
                         nr = length ms
                     in Node { player   = p
                             , state    = g
                             , movesnr  = nr
                             , childid  = 0
                             , mov      = array (0, nr - 1) $ zip [0 ..] $ ms
                             , children = array (0, nr - 1) $ map (\(i, m) -> (i, (child m) {childid = i})) $ zip [0 ..] ms
                             , val      = value prop p g
                             , best     = []
                             , filled   = False
                             , closed   = False
                             , mind     = 0
                             , maxd     = 0
                             , volume   = 0
                             }
  where
--  child :: Move g -> Tree g
    child m = uncurry (buildtree prop) (m (p, g))
    
createtree :: Game g => g -> Properties -> Tree g
createtree _ p = buildtree p 0 $ new p

-----------------------
-- Tree manipulation --
-----------------------

better :: Player -> Value -> Value -> Ordering
better p v w | p < 0 || p >= length v || p >= length w = error $ "Game.better: index " ++ show p ++ " out of bounds"
             | otherwise                               = compare (v !! p) (w !! p)

computeVal :: Game g => Tree g -> Tree g
computeVal t = let kids = assocs $ children t                    
                   vals = map (\(i, k) -> (i, val k)) kids
                   good = maximumWith (\(_,v) (_,w) -> better (player t) v w) vals
               in t { best = map fst good
                    , val  = zipWithn average $ map snd good --{ veranderen als val set is ipv list
                    }

-- shear cuts a branch off and makes it the new tree
shear :: Game g => Int -> Tree g -> Tree g
shear i t = children t ! i

-- grow makes the tree grow at its root (which should be a leaf)
grow :: Game g => Tree g -> Tree g
grow t | movesnr t == 0 = t {closed = True, filled = True, mind = inf, maxd = 1, volume = 1}
       | otherwise      = computeVal $ t   {filled = True, mind = 1  , maxd = 1, volume = 1}

-- update recomputes val, best, mind and maxd given the index of the altered child
--{ update moet efficienter dan altijd computeVal!
update :: Game g => Int -> Tree g -> Tree g
update _i t = computeVal
           $ t { closed = and $ map closed $ elems $ children t
               , mind   = case filter (not . closed) $ elems $ children t
                          of []   -> inf
                             kids -> minimum $ map ((+ 1) . mind) kids
               , maxd   = maximum $ map ((+ 1) . maxd) $ elems $ children t
               , volume = 1 + sum (map volume $ elems $ children t)
               }

-- path computes a path to a leaf to update, given a leaf-choosing algorithm f
path :: Game g => (Tree g -> [Int]) -> [Int] -> Tree g -> [Int]
path f (j:js) t = case f t of [] -> []
                              is -> let i = is !! (j `mod` length is)
                                    in i : path f js (children t ! i)
path _ []     _ = error "path: empty list"

-- step makes the tree grow at exactly one leaf, given a path
step :: Game g => [Int] -> Tree g -> Tree g
step []     t = grow t
step (i:is) t 
  | filled t  = let u = step is (children t ! i)
                in  update i $ t {children = children t // [(i, u)]}
  | otherwise = grow t

followcombination :: Game g => Tree g -> [Int]
followcombination t = followshortest t ++ followbest t

followshortest :: Game g => Tree g -> [Int]
followshortest t | not $ filled t = []
                 | closed t       = []
                 | otherwise      = let open = filter (\(_i, k) -> not $ closed k) $ assocs $ children t
                                        minds = map (\(i, k) -> (i, mind k)) open
                                    in map fst $ minimumWith (\(_, p) (_, q) -> compare p q) minds

followbest :: Game g => Tree g -> [Int]
followbest t = case filter (\i -> not $ closed $ children t ! i) $ best t of
                 [] -> followopen t
                 b  -> b
                 
followopen :: Game g => Tree g -> [Int]
followopen t = map fst $ filter (not.closed.snd) $ assocs $ children t