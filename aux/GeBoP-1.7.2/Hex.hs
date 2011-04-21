

{-{ http://en2.wikipedia.org/wiki/Hex_(game) }-}


---------
-- Hex --
---------

module Hex (Hex, hex) where

import Game
import Array
import Graphics.UI.WX     hiding (border, point)
import Graphics.UI.WXCore hiding (point)
import Tools

data Hex = Hex (Array (Int, Int) (Maybe Player)) deriving (Eq, Show)

type HexMove = (Int, Int)

hex :: Hex
hex = undefined

instance Game Hex where

  name _ = "hex"

  standard _ = Properties    { players = 2, boardsize = 11, human = [True, False] }
  possible _ = PropertyRange { playersrange = [2], boardsizerange = [5 .. 15] }

  new pr = let bsz = boardsize pr
           in Hex $ array ((0, 0), (bsz - 1, bsz - 1)) [((x, y), Nothing) | x <- [0 .. bsz - 1], y <- [0 .. bsz - 1]]
  
  moves pr p (Hex st) = map (move pr) (allMoves pr p st)

  showmove pr p (Hex s) i = case allMoves pr p s !! i
                            of (x, y) -> ['a' ..] !! x : show (1 + y)

  value pr _p (Hex st) 
    | win st (boardsize pr) 0 = [ 1, -1]
    | win st (boardsize pr) 1 = [-1,  1]
    | otherwise               = [ 0,  0]


  board p pr vart _ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg <- varCreate $ grate rectZero 0 (0, 0) sizeZero

    let 

      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        t <- varGet vart
        let Hex st = state t
            bsz = boardsize pr
        b <- border dc (bsz, bsz)
        let g = grate r b (2 * bsz + 1, 2 * bsz + 2) (Size 7 4)
            radius = rectWidth (field g (0, 0)) `div` 3
            lin' :: Rect -> Rect -> Color -> IO ()
            lin' (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) c = do
              line dc (pt (x1 + w1 `div` 2) (y1 + h1)) (pt (x2 + w2 `div` 2) (y2 + h2)) [penWidth := 4, penColor := c]
            lin :: (Int, Int) -> (Int, Int) -> Color -> IO ()
            lin p' q = lin' (field g $ tograte bsz p') (field g $ tograte bsz q)
        varSet varg g
        tileBitmap dc r marble
--{        drawGrate dc g [penColor := yellow]
        for 0 (bsz - 1) (\i -> do
          drawTextRect dc [['A' ..] !! i] $ field g (bsz + 2 + i, 2 * bsz + 1 - i) |#| field g (bsz + 2 + i, 2 * bsz + 2 - i)
          drawTextRect dc (show (i + 1))  $ field g (bsz - 2 - i, 2 * bsz + 1 - i) |#| field g (bsz - 2 - i, 2 * bsz + 2 - i)
          )
        lin (0, -1) (bsz - 1, -1) blue
        lin (0, bsz) (bsz - 1, bsz) blue
        lin (-1, 0) (-1, bsz - 1) red
        lin (bsz, 0) (bsz, bsz - 1) red
        for 0 (bsz - 1) (\i -> for 0 (bsz - 1) (\j ->
          drawPiece dc (field g $ tograte bsz (i, j)) radius (st ! (i, j))
                   ) )

      onclick :: Point -> IO ()
      onclick point = do 
        t <- varGet vart
        g <- varGet varg
        let Hex st = state t
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
drawPiece dc (Rect x y w h) r mp = do
  circle dc (pt (x + w `div` 2) (y + h)) (max 1 (r `div` 5)) [brushColor := black]
  case mp of Just 0  -> circle dc (pt (x + w `div` 2) (y + h)) r [brushKind := BrushTransparent, penColor := blue, penWidth := 3]
             Just 1  -> circle dc (pt (x + w `div` 2) (y + h)) r [brushKind := BrushTransparent, penColor := red , penWidth := 3]
             Nothing -> return ()
             _       -> error "drawPiece: Unexpected value"

tograte :: Int -> (Int, Int) -> (Int, Int)
tograte bsz (i, j) = (bsz + i - j, 2 * bsz - 1 - i - j)

fromgrate :: Int -> (Int, Int) -> (Int, Int)
fromgrate bsz (i, j) = ((bsz + i - j) `div` 2, (3 * bsz - i - j) `div` 2)

{-
i = s - 1 + x - y
j = 2s - 2 - x - y

i + j = 3s - 3 - 2y
2y = 3s - 3 - i - j
y = (...) / 2

i - j = -s + 1 + 2x
2x = s - 1 + i - j
-}

(+-) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) +- (c, d) = (a + c, b + d)

allMoves :: Properties -> Player -> Array (Int, Int) (Maybe Player) -> [HexMove]
allMoves pr p st 
  | win st (boardsize pr) (1 - p) = []
  | otherwise = map fst $ filter ((== Nothing) . snd) $ assocs st

move :: Properties -> HexMove -> (Player, Hex) -> (Player, Hex)
move _pr place (p, Hex s) = (1 - p, Hex $ s // [(place, Just p)])

win :: Array (Int, Int) (Maybe Player) -> Int -> Player -> Bool
win st bsz 0 = any ((== bsz - 1) . snd) $ floodfill st 0 (zip [0 .. bsz - 1] [-1, -1 ..])
win st bsz 1 = any ((== bsz - 1) . fst) $ floodfill st 1 (zip [-1, -1 ..] [0 .. bsz - 1])
win _  _   _ = error "win: Unexpected value"
   
floodfill :: Array (Int, Int) (Maybe Player) -> Player -> [(Int, Int)] -> [(Int, Int)]
floodfill st p togo = floodfill_ p togo []
 where
  floodfill_ :: Player -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
  floodfill_ _  []          known = known
  floodfill_ p' (f : togo') known = let new' = filter (not . flip elem known) $ map (f +-) steps
                                        good = filter (\f' -> st ! f' == Just p') $ filter (inRange (bounds st)) new'
                                    in floodfill_ p' (togo' ++ good) (known ++ good)

steps :: [(Int, Int)]
steps = [(1, 1), (1, 0), (0, -1), (-1, -1), (-1, 0), (0, 1)]

{-
jumps :: [((Int, Int), [(Int, Int)])]
jumps = [ (( 2,  1), [( 1,  0), ( 1,  1)])
        , (( 1,  2), [( 0,  1), ( 1,  1)])
        , ((-1,  1), [( 0,  1), (-1,  0)])
        , ((-2, -1), [(-1,  0), (-1, -1)])
        , ((-1, -2), [( 0, -1), (-1, -1)])
        , (( 1, -1), [( 1,  0), ( 0, -1)])
        ]
-}

{- the hexboard internally look like this:


                  *
               *     *
            *     *     *
         *     *     *     *
      *     *     *     *     *
   *     *     *     *     *     *
*     *     *     *     *     *     *
   *     *     *     *     *     *
      j     *     *     *     i
         *     *     *     *
            2     *     2
               1     1
                  0     
 
-}