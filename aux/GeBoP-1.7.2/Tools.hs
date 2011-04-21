
-----------
-- Tools --
-----------

module Tools 
  ( module HSL
  , module Inf
  , module Tools
  ) where

import Random
import List

import Graphics.UI.WX
import Graphics.UI.WXCore

import HSL
import Inf

-- list functions --

maximumWith, minimumWith :: (a -> a -> Ordering) -> [a] -> [a]
maximumWith _ []       = []
maximumWith p (x : xs) = case maximumWith p xs
                         of []       -> [x]
                            (y : ys) -> case p x y
                                        of GT -> x :     []
                                           EQ -> x : y : ys
                                           LT ->     y : ys
minimumWith = maximumWith . flip

(|>) :: Int -> a -> [a] -> [a]
(|>) 0 y (_ : xs) = y : xs
(|>) n y (x : xs) = x : (|>) (n - 1) y xs
(|>) _ _ _        = error "(|>): index out of bounds"

(!!!) :: Show a => [a] -> Int -> a
es !!! i | i < 0 || i >= length es = error $ show i ++ " !!! " ++ show es
         | otherwise               = es !! i

sepWith :: [a] -> [[a]] -> [a]
sepWith _ []         = []
sepWith _ [as]       = as
sepWith s (as : ass) = as ++ s ++ sepWith s ass

zipWithn :: ([a] -> b) -> [[a]] -> [b]
zipWithn f = map f . transpose

randomList :: IO [Int]
randomList = do i <- randomIO
                let g = mkStdGen i
                return $ randoms g

randomElement :: [a] -> IO a
randomElement [] = error "randomElement: empty list"
randomElement xs = do i <- randomRIO (0, length xs - 1)
                      return $ xs !! i

-- io functions --

for :: Int -> Int -> (Int -> IO ()) -> IO ()
for x y f = sequence_ $ map f [x..y]

ifIO :: (IO Bool) -> IO () -> IO ()
ifIO iob io = do b <- iob
                 if b then io else return ()

varCopy :: Var a -> IO (Var a)
varCopy = flip (>>=) varCreate . varGet

-- other --

numberword :: Int -> String
numberword x | x < 0 = "minus " ++ numberword (negate x)
numberword  0 = "zero"
numberword  1 = "one"
numberword  2 = "two"
numberword  3 = "three"
numberword  4 = "four"
numberword  5 = "five"
numberword  6 = "six"
numberword  7 = "seven"
numberword  8 = "eight"
numberword  9 = "nine"
numberword 10 = "ten"
numberword 11 = "eleven"
numberword 12 = "twelve"
numberword 13 = "thirteen"
numberword 15 = "fifteen"
numberword 18 = "eighteen"
numberword x | x < 20 = numberword (x - 10) ++ "teen"
numberword 20 = "twenty"
numberword 30 = "thirty"
numberword 40 = "forty"
numberword 50 = "fifty"
numberword 80 = "eighty"
{-
numberword x |           x < 10 ^  2 = largenumber 1 "ty"        "-" x
             |           x < 10 ^  3 = largenumber 2 " hundred"  " " x
             |           x < 10 ^  6 = largenumber 3 " thousand" " " x
             |           x < 10 ^  9 = largenumber 6 " million"  " " x
             | toInteger x < 10 ^ 12 = largenumber 9 " billion"  " " x
-}
numberword x | smallerExp10 x 2  = largenumber 1 "ty"        "-" x
             | smallerExp10 x 3  = largenumber 2 " hundred"  " " x
             | smallerExp10 x 6  = largenumber 3 " thousand" " " x
             | smallerExp10 x 9  = largenumber 6 " million"  " " x
             | smallerExp10 x 12 = largenumber 9 " billion"  " " x
  where
    smallerExp10 :: Int -> Int -> Bool
    smallerExp10 y z = y < 10 ^ z

    largenumber :: Int -> String -> String -> Int -> String
    largenumber q s t y
      | y `mod` (10 ^ q) == 0 = numberword (y `div` (10 ^ q)) ++ s
      | otherwise = numberword (y - y `mod` (10 ^ q)) ++ t ++ numberword (y `mod` (10 ^ q))
numberword _ = "unknown number"

sqr :: Int -> Int
sqr x = x * x

average :: [Float] -> Float
average xs = sum xs / (fromInteger . toInteger) (length xs)

-- wx functions --

type Wire = Graphics.UI.WX.Timer

wire :: Window a -> [Prop Wire] -> IO Wire
wire f ps = do w <- timer f ps
               timerStop w
               return w

send :: Wire -> IO ()
send w = do _ <- timerStart w 1 True
            return ()

wait :: Window a -> Int -> IO () -> IO ()
wait w n action = do
  t <- timer w []
  set t [interval := n, on command := set t [enabled := False] >> action]
  

cut :: Ord a => (a, a) -> a -> a
cut (l, u) x 
  | x < l     = l
  | x > u     = u
  | otherwise = x


tileBitmap :: DC () -> Rect -> Bitmap () -> IO ()
tileBitmap dc (Rect _x _y w h) bmp = do
  bw <- bitmapGetWidth  bmp
  bh <- bitmapGetHeight bmp
  for 0 (w `div` bw) (\i ->
    for 0 (h `div` bh) (\j ->
      drawBitmap dc bmp (pt (i * bw) (j * bh)) False []))

-- grates -- 

data Grate = Grate Rect (Int, Int) Int

grate :: Rect -> Int -> (Int, Int) -> Size -> Grate
grate (Rect x y w h) b (m, n) (Size u v) =
  grate_ (Rect (x + b) (y + b) (w - 2 * b) (h - 2 * b))
  where
    grate_ :: Rect -> Grate
    grate_ (Rect x' y' w' h') =
      let t = min (w' * n * v) (h' * m * u)
          w_ = t `div` (n * v)
          h_ = t `div` (m * u)
          x_ = (x' + (w' - w_) `div` 2)
          y_ = (y' + (h' - h_) `div` 2) 
      in Grate (Rect x_ y_ w_ h_) (m, n) b


field :: Grate -> (Int, Int) -> Rect
field (Grate (Rect x y w h) (m, n) _) (i, j) =
  let fx, fy :: Int -> Int
      fx i' = x + i' * w `div` m
      fy j' = y + j' * h `div` n
  in Rect (fx i) (fy j) (fx (i + 1) - fx i) (fy (j + 1) - fy j)

locate :: Grate -> Point -> (Int, Int)
locate (Grate (Rect x y w h) (m, n) _) (Point px py) =
  ((px - x) * m `div` w, (py - y) * n `div` h)

drawGrate :: DC () -> Grate -> [Prop (DC ())] -> IO ()
drawGrate dc g@(Grate _ (m, n) _) options =
  for 0 (m - 1) (\i -> for 0 (n - 1) (\j ->
    drawRect dc (field g (i, j)) options
                )                    )
                
edge :: Grate -> (Int, Int) -> Rect
edge g@(Grate _ (m, n) b) (i, j) =
  let Rect x y w h = field g (i, j)
      x_ | i == -1   = x + w - b
         | otherwise = x
      y_ | j == -1   = y + h - b
         | otherwise = y
      w_ | i < 0 || i >= m = b
         | otherwise       = w
      h_ | j < 0 || j >= n = b
         | otherwise       = h
  in Rect x_ y_ w_ h_
  
drawTextRect :: DC () -> String -> Rect -> IO ()
drawTextRect dc s (Rect x y w h) = do
  Size u v <- getTextExtent dc s
  drawText dc s (pt (x + (w - u) `div` 2) (y + (h - v) `div` 2)) []
    
border :: DC () -> (Int, Int) -> IO Int
border dc (m, n) = do
  let ms = map show [1 .. m]
      ns = map (:[]) $ take n ['A' ..]
  hs <- sequence $ map (getTextExtent dc) ms
  ws <- sequence $ map (getTextExtent dc) ns
  return $ maximum (map sizeH hs ++ map sizeW ws)
  
bounding :: Rect -> Rect -> Rect
bounding (Rect x y w h) (Rect x_ y_ w_ h_) =
  let r = x + w; r_ = x_ + w_
      b = y + h; b_ = y_ + h_
  in rectBetween (pt (min x x_) (min y y_)) (pt (max r r_) (max b b_))

(|#|) :: Rect -> Rect -> Rect
(|#|) = bounding
