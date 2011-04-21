

{-{ http://en2.wikipedia.org/wiki/Zenix_(game) }-}


-----------
-- Zenix --
-----------

module Zenix (Zenix, zenix) where

import Game
import Array
import Graphics.UI.WX     hiding (border, point)
import Graphics.UI.WXCore hiding (point)
import Tools

data Zenix = Zenix (Array (Int, Int) (Maybe Player)) deriving (Eq, Show)

type ZenixMove = (Int, Int)

zenix :: Zenix
zenix = undefined

instance Game Zenix where

  name _ = "zenix"

  standard _ = Properties    { players = 2, boardsize = 8, human = [True, False, False] }
  possible _ = PropertyRange { playersrange = [2, 3], boardsizerange = [4 .. 12] }

  new pr = let bsz = boardsize pr
           in Zenix $ array ((0, 0), (bsz - 1, bsz - 1)) [((x, y), Nothing) | x <- [0 .. bsz - 1], y <- [0 .. bsz - 1]]
  
  moves pr p (Zenix st) = map (move pr) (allMoves pr p st)

  showmove pr p (Zenix s) i = case allMoves pr p s !! i
                            of (x, y) -> ['a' ..] !! x : show (1 + y)

  value pr p (Zenix st) 
    | null $ allMoves pr p st = let winners = map fst $ filter (\(_i, c) -> c == maximum chains) $ zip [0 ..] chains
                                in  foldr ($) (replicate (players pr) (-1)) $ map (|> 1) winners
    | otherwise               = map myvalue [0 .. players pr - 1]
   where
    bsz = boardsize pr

    chains :: [Int]
    chains = map (\p' -> scan p' bsz []) [0 .. players pr - 1]

    myvalue :: Player -> Float
    myvalue p' = let t = fromInteger . toInteger $ (players pr) * (chains !! p') - sum chains
                     n = fromInteger . toInteger $ 2 * bsz
                in t / n

    scan :: Player -> Int -> [Int] -> Int
    scan p' y _ | y >= bsz = scan p' (bsz - 1) [0 .. bsz]
    scan _p y [] = bsz - y
    scan p' y xs = scan p' (y - 1) $ filter good [0 .. y]
     where
      good :: Int -> Bool
      good x = st ! (x, y) == Just p'
            && (x `elem` xs || (x + 1) `elem` xs)

  board p pr vart _ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg <- varCreate $ grate rectZero 0 (0, 0) sizeZero

    let 

      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        t <- varGet vart
        let Zenix st = state t
            bsz = boardsize pr
        b <- border dc (bsz, bsz)
        let g = grate r b (2 * bsz, bsz) (Size 4 7)
            radius = rectWidth (field g (0, 0))
{-
            lin' :: Rect -> Rect -> Color -> IO ()
            lin' (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) c = do
              line dc (pt (x1 + w1 `div` 2) (y1 + h1)) (pt (x2 + w2 `div` 2) (y2 + h2)) [penWidth := 4, penColor := c]
            lin :: (Int, Int) -> (Int, Int) -> Color -> IO ()
            lin p q = lin' (field g $ tograte bsz p) (field g $ tograte bsz q)
-}
        varSet varg g
        tileBitmap dc r marble
--{        drawGrate dc g [penColor := yellow]
        for 0 (bsz - 1) (\i -> do
          drawTextRect dc [['A' ..] !! i] $ edge g (2 * i, bsz)
          drawTextRect dc (show (i + 1))  $ field g (i - 1, bsz - i - 1)
          )
        for 0 (bsz - 1) (\i -> for 0 (bsz - 1) (\j ->
          when (j - i >= 0) $ drawPiece dc (field g $ tograte bsz (i, j)) radius (st ! (i, j))
          ))

      onclick :: Point -> IO ()
      onclick point = do 
        t <- varGet vart
        g <- varGet varg
        let Zenix st = state t
            bsz = boardsize pr
            n = fromgrate bsz $ locate g point 
        case lookup n $ zip (allMoves pr (player t) st) [0..] of
            Nothing -> return ()
            Just  i -> move' i

    set p [ on click    := onclick
          , on paint    := onpaint
          , on resize  ::= repaint
          ]

    return ()

drawPiece :: DC () -> Rect -> Int -> Maybe Player -> IO ()
drawPiece dc (Rect x y _w h) r mp = do
  case mp of Just 0  -> circle dc (pt x (y + h `div` 2)) r [brushKind := BrushSolid, brushColor := blue ]
             Just 1  -> circle dc (pt x (y + h `div` 2)) r [brushKind := BrushSolid, brushColor := red  ]
             Just 2  -> circle dc (pt x (y + h `div` 2)) r [brushKind := BrushSolid, brushColor := green]
             Nothing -> circle dc (pt x (y + h `div` 2)) r [brushKind := BrushTransparent]
             _       -> error "drawPiece: Unexpected value"

{-
(+-) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) +- (c, d) = (a + c, b + d)
-}

allMoves :: Properties -> Player -> Array (Int, Int) (Maybe Player) -> [ZenixMove]
allMoves pr _p st 
  | otherwise = filter free $ indices st
 where
  bsz = boardsize pr
  free :: ZenixMove -> Bool
  free (x, y) = y - x >= 0
             && st ! (x, y) == Nothing
             && (y == bsz - 1 || (st ! (x, y + 1) /= Nothing && st ! (x + 1, y + 1) /= Nothing))

move :: Properties -> ZenixMove -> (Player, Zenix) -> (Player, Zenix)
move pr place (p, Zenix s) = ( (p + 1) `mod` players pr
                             , Zenix $ s // [(place, Just p)]
                             )

{-{
win :: Array (Int, Int) (Maybe Player) -> Int -> Player -> Bool
win st bsz 0 = any ((== bsz - 1) . snd) $ floodfill st 0 (zip [0 .. bsz - 1] [-1, -1 ..])
win st bsz 1 = any ((== bsz - 1) . fst) $ floodfill st 1 (zip [-1, -1 ..] [0 .. bsz - 1])
   
floodfill :: Array (Int, Int) (Maybe Player) -> Player -> [(Int, Int)] -> [(Int, Int)]
floodfill st p togo = floodfill_ p togo []
 where
  floodfill_ :: Player -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
  floodfill_ p []         known = known
  floodfill_ p (f : togo) known = let new  = filter (not . flip elem known) $ map (f +-) steps
                                      good = filter (\f -> st ! f == Just p) $ filter (inRange (bounds st)) new
                                  in floodfill_ p (togo ++ good) (known ++ good)

steps :: [(Int, Int)]
steps = [(1, 1), (1, 0), (0, -1), (-1, -1), (-1, 0), (0, 1)]

jumps :: [((Int, Int), [(Int, Int)])]
jumps = [ (( 2,  1), [( 1,  0), ( 1,  1)])
        , (( 1,  2), [( 0,  1), ( 1,  1)])
        , ((-1,  1), [( 0,  1), (-1,  0)])
        , ((-2, -1), [(-1,  0), (-1, -1)])
        , ((-1, -2), [( 0, -1), (-1, -1)])
        , (( 1, -1), [( 1,  0), ( 0, -1)])
        ]
}-}

tograte :: Int -> (Int, Int) -> (Int, Int)
tograte bsz (i, j) = (bsz + 2 * i - j, j)

fromgrate :: Int -> (Int, Int) -> (Int, Int)
fromgrate bsz (i, j) = ((i + j - bsz + 1) `div` 2, j)

{-
i = s - 1 + x - y
j = 2s - 2 - x - y

i + j = 3s - 3 - 2y
2y = 3s - 3 - i - j
y = (...) / 2

i - j = -s + 1 + 2x
2x = s - 1 + i - j
-}


{- the zenixboard internally look like this:

0      *
1     * *
2    * * *
3   * * * *
4  * * * * *
5 * * * * * *
 0 1 2 3 4 5
 
-}