
-----------
-- Halma --
-----------

module Halma (Halma, halma) where

import Game
import Array
-- import Graphics.UI.WX
import Graphics.UI.WX     hiding (border, empty, point)
import Graphics.UI.WXCore hiding (empty, point)
import Tools

data Halma = Halma (Array (Int, Int) (Maybe Player)) deriving (Eq, Show)

type HalmaMove = ((Int, Int), (Int, Int))

halma :: Halma
halma = undefined

instance Game Halma where

  name _ = "halma"

  standard _ = Properties    { players = 2, boardsize = 8, human = [True, False, False, False, False, False] }
  possible _ = PropertyRange { playersrange = [2, 3, 4, 6], boardsizerange = [8] }

  new pr = let empty = [((x, y), Nothing) | x <- [-8 .. 8], y <- [-8 .. 8]]
           in Halma $ array ((-8, -8), (8, 8)) empty // concatMap (\p -> map (\t -> (t, Just p)) $ startpos $ pos pr p) [0 .. players pr - 1]
  
  moves pr p (Halma s) = map (move pr) (allMoves pr p s)

  showmove pr p (Halma s) i = let ((x1, y1), (x2, y2)) = allMoves pr p s !! i
                              in "abcdefghijklmnopq" !! (x1 + 8) : show (9 - y1) ++ "-" ++ "abcdefghijklmnopq" !! (x2 + 8) : show (9 - y2)
  
  value pr p (Halma st) | null $ allMoves pr p st = let winners = map snd $ filter (\(d, _) -> d == 20) $ zip totaldists [0..]
                                                    in foldr ($) (replicate (players pr) (-1)) $ map (|> 1) winners
                        | otherwise               = map myvalue [0 .. players pr - 1]
    where
      totaldists :: [Int]
      totaldists = map totaldist [0 .. players pr - 1]

      totaldist :: Player -> Int
      totaldist p' = let mypieces = map (\(i, _e) -> i) $ filter (\(_i, e) -> e == Just p') $ assocs st
                     in sum $ map (dist pr p') mypieces

      myvalue :: Player -> Float
      myvalue p' = let d = sum (map totaldist [0 .. players pr - 1]) - (players pr) * totaldist p'
                   in (fromInteger . toInteger) d  / (fromInteger . toInteger) (120 * (players pr))

  board p pr vart _ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg   <- varCreate $ grate rectZero 0 (0, 0) sizeZero
    vare   <- varCreate (Nothing :: Maybe (Int, Int))

    let 
    
      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        t <- varGet vart
        e <- varGet vare
        b_ <- border dc (16, 16)
        let g_ = grate r b_ (26, 17) (Size 4 7)
        b <- fit dc (16, 16) $ rectWidth (field g_ (0, 0))
        let Halma st = state t
            g = grate r b (26, 17) (Size 4 7)
            radius = rectHeight (field g (0, 0)) `div` 3
            lin' :: Rect -> Rect -> IO ()
            lin' (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) = do
              line dc (pt (x1 + w1) (y1 + h1 `div` 2)) (pt (x2 + w2) (y2 + h2 `div` 2)) []
            lin :: (Int, Int) -> (Int, Int) -> IO ()
            lin p' q = lin' (field g $ tograte p') (field g $ tograte q)
        varSet varg g
        tileBitmap dc r marble
--{        drawGrate dc g [penColor := yellow]
        for 0 16 (\j -> do
          let i = head $ dropWhile (\i' -> inside $ fromgrate (i', j)) [13 ..]
          drawTextRect dc (show $ 17 - j) $ field g ( i - 1, j) |#| field g (     i, j)
          drawTextRect dc (show $ 17 - j) $ field g (25 - i, j) |#| field g (26 - i, j)
          let d  = (i  - 1 + 3 * j) `div` 2 - 18
              e' = (25 - i + 3 * j) `div` 2 - 18
          drawTextRect dc [['A' ..] !! (16 - j)] $ field g (i  - 1 - d, j - d) |#| field g (i  - 1 - d, j - 1 - d)
          drawTextRect dc [['A' ..] !! (16 - j)] $ field g (25 - i - e', j - e') |#| field g (25 - i - e', j - 1 - e')
          )
        for 0 4 (\n -> do
          lin (  - 4,  n - 8) (n - 4,  n - 8)
          lin (n - 8,  n - 4) (    4,  n - 4)
          lin (  - 4,  n    ) (n + 4,  n    )
          lin (n    ,  n + 4) (    4,  n + 4)
          lin (n - 8,    - 4) (n - 8,  n - 4)
          lin (n - 4,  n - 8) (n - 4,      4)
          lin (n    ,    - 4) (n    ,  n + 4)
          lin (n + 4,  n    ) (n + 4,      4)
          lin (  - 4, -n + 4) (n - 4,      4)
          lin (n - 8,    - 4) (    4, -n + 8)
          lin (  - 4, -n - 4) (n + 4,      4)
          lin (n    ,    - 4) (    4, -n    )
          )
        for 0 24 (\i -> for 0 16 (\j ->
          when (even (i + j)) $ when (inside $ fromgrate (i, j)) $
            drawPiece dc (field g (i, j)) radius (st ! fromgrate (i, j))
                   ) )
        case e of Just p' -> drawBrightPiece dc (field g $ tograte p') radius
                  Nothing -> return ()
         
      onclick :: Point -> IO ()
      onclick point = do 
        t <- varGet vart
        e <- varGet vare
        g <- varGet varg
        let Halma st = state t
            n = fromgrate $ locate g point 
        case (e, inside n) of
          (Nothing, True ) -> when (st ! n == Just (player t)) $ varSet vare (Just n) >> repaint p
          (_      , False) -> varSet vare Nothing >> repaint p
          (Just te, True ) -> case lookup (te, n) $ zip (allMoves pr (player t) st) [0..] of
            Nothing -> varSet vare Nothing >> repaint p
            Just  i -> varSet vare Nothing >> repaint p >> move' i

    set p [ on click    := onclick
          , on unclick  := onclick
          , on paint    := onpaint
          , on resize  ::= repaint
          ]

    return ()

fit :: DC () -> (Int, Int) -> Int -> IO Int
fit dc t m = do
  s <- get dc fontSize
  fit_ dc (s + 6)
 where
  fit_ :: DC () -> Int -> IO Int
  fit_ _dc 1 = border dc t
  fit_ dc' s = do
    set dc' [fontSize := s - 1]
    b <- border dc t
    if b <= m then return b
              else fit_ dc' (s - 1)

drawPiece :: DC () -> Rect -> Int -> Maybe Player -> IO ()
drawPiece dc (Rect x y w h) r mp = circle dc (pt (x + w) (y + h `div` 2)) r [brushColor := col mp]

drawBrightPiece :: DC () -> Rect -> Int -> IO ()
drawBrightPiece dc (Rect x y w h) r = circle dc (pt (x + w) (y + h `div` 2)) r [brushKind := BrushTransparent, penWidth := 3, penColor := yellow]

tograte :: (Int, Int) -> (Int, Int)
tograte (i, j) = (12 + 2 * i - j, 8 + j)

fromgrate :: (Int, Int) -> (Int, Int)
fromgrate (i, j) = (-10 + (i + j) `div` 2, -8 + j)

-- x = ½ (i + j) - 10
-- y = j - 8

col :: Maybe Player -> Color
col p = case p of 
  Nothing -> white
  Just 0  -> blue
  Just 1  -> red
  Just 2  -> green
  Just 3  -> rgb 160 0   (192 :: Int)
  Just 4  -> rgb 192 128 (0 :: Int)
  Just 5  -> grey
  _       -> black

(+-) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) +- (c, d) = (a + c, b + d)

allMoves :: Properties -> Player -> Array (Int, Int) (Maybe Player) -> [HalmaMove]
allMoves pr p st | p == 0 && 20 `elem` (map totaldist [0 .. players pr - 1]) = []
                 | otherwise = stepmoves ++ jumpmoves
  where
    mypieces :: Player -> [(Int, Int)]
    mypieces p' = map (\(i, _e) -> i) $ filter (\(_i, e) -> e == Just p') $ assocs st

    stepmoves :: [HalmaMove]
    stepmoves = let potmoves = concatMap (\t -> map (\s -> (t, t +- s)) $ steps pr p) (mypieces p)
                in filter (\(_f, t) -> inside t && st ! t == Nothing) potmoves

    jumpmoves :: [HalmaMove]
    jumpmoves =  concatMap (\t -> map (\s -> (t, s)) $ floodfill t []) (mypieces p)

    floodfill :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    floodfill t fs = let news = map (\j -> t +- j +- j)
                              $ filter (\j -> let u = t +- j +- j
                                              in inside u 
                                              && st ! (t +- j) /= Nothing
                                              && st ! u == Nothing
                                              && not (u `elem` fs)
                                       )
                              $ halfjumps
                     in foldr ($) fs $ map (\u -> floodfill u . (u :)) news
    totaldist :: Player -> Int
    totaldist p' = sum $ map (dist pr p') $ mypieces p'

steps :: Properties -> Player -> [(Int, Int)]
steps pr p = steppos (pos pr p)
  where
    steppos 0 = [( 1,  1), ( 1,  0), ( 0, -1), (-1, -1)]
    steppos 1 = [( 0,  1), ( 1,  1), ( 1,  0), ( 0, -1)]
    steppos 2 = [(-1,  0), ( 0,  1), ( 1,  1), ( 1,  0)]
    steppos 3 = [(-1, -1), (-1,  0), ( 0,  1), ( 1,  1)]
    steppos 4 = [( 0, -1), (-1, -1), (-1,  0), ( 0,  1)]
    steppos 5 = [( 1,  0), ( 0, -1), (-1, -1), (-1,  0)]
    steppos _ = error "steps: Unexpected value"

{-
jumps :: [(Int, Int)]
jumps = map (\(x, y) -> (2 * x, 2 * y)) halfjumps
-}

halfjumps :: [(Int, Int)]
halfjumps = [(1, 1), (1, 0), (0, -1), (-1, -1), (-1, 0), (0, 1)]

dist :: Properties -> Player -> (Int, Int) -> Int
dist pr p t = distpos (pos pr p) t
  where
    distpos 0 (x, y) = 8 - x + y + max 0 (-4 + x    ) + max 0 (-4 - y    )
    distpos 1 (x, y) = 8 - x     + max 0 (-4 + x - y) + max 0 (-4 + y    )
    distpos 2 (x, y) = 8     - y + max 0 (-4 + x    ) + max 0 (-4 + y - x)
    distpos 3 (x, y) = 8 + x - y + max 0 (-4 - x    ) + max 0 (-4 + y    )
    distpos 4 (x, y) = 8 + x     + max 0 (-4 - x + y) + max 0 (-4 - y    )
    distpos 5 (x, y) = 8     + y + max 0 (-4 - x    ) + max 0 (-4 - y + x)
    distpos _ _      = error "dist: Unexpected value"

move :: Properties -> HalmaMove -> (Player, Halma) -> (Player, Halma)
move pr (f, t) (p, Halma s) = ( (p + 1) `mod` players pr
                              , Halma $ s // [(f, Nothing), (t, Just p)]
                              )

startpos :: (Num t, Num t1, Enum t1) => t -> [(t1, t1)]
startpos 0 = [(x, y) | x <- [-4 .. -1], y <- [x + 5 ..     4]]
startpos 1 = [(x, y) | x <- [-8 .. -5], y <- [  - 4 .. x + 4]]
startpos 2 = [(x, y) | x <- [-4 .. -1], y <- [x - 4 ..   - 5]]
startpos 3 = [(x, y) | x <- [ 1 ..  4], y <- [  - 4 .. x - 5]]
startpos 4 = [(x, y) | x <- [ 5 ..  8], y <- [x - 4 ..     4]]
startpos 5 = [(x, y) | x <- [ 1 ..  4], y <- [    5 .. x + 4]]
startpos _ = error "startpos: Unexpected value"

pos :: Properties -> Player -> Int
pos pr p | players pr == 2 = [0, 3      ] !! p
         | players pr == 3 = [0, 2, 4   ] !! p
         | players pr == 4 = [0, 1, 3, 4] !! p
         | players pr == 6 = p
         | otherwise       = error "pos: Unexpected value"

inside :: (Int, Int) -> Bool
inside (x, y) = (x >= -4 && y <= 4 && x <= y + 4)
             || (y >= -4 && x <= 4 && y <= x + 4)

{- the halmaboard internally looks like this:

y/j

-8 ....x............
-7 ....xx...........
-6 ....xxx..........
-5 ....xxxx.........
-4 xxxx*****xxxx....
-3 .xxx******xxx....
-2 ..xx*******xx....
-1 ...x********x....
 0 ....*********....
 1 ....x********x...
 2 ....xx*******xx..
 3 ....xxx******xxx.
 4 ....xxxx*****xxxx
 5 .........xxxx....
 6 ..........xxx....
 7 ...........xx....
 8 ............x....

   87654321012345678 x/i
   --------
  
-}