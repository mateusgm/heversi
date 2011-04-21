
----------
-- Bamp --
----------

module Bamp (Bamp, bamp) where

import Game
import Array
import Graphics.UI.WX     hiding (border, point)
import Graphics.UI.WXCore hiding (point)
import Tools

data BampField
  = Empty
  | Portal Player
  | Piece Player
  | Ball
  deriving (Eq, Show)

isPortal :: BampField -> Bool
isPortal (Portal _) = True
isPortal _          = False

{-
isPiece :: BampField -> Bool
isPiece (Piece _) = True
isPiece _         = False
-}

data Bamp = Bamp (Array (Int, Int) BampField) deriving (Eq, Show)

bamp :: Bamp
bamp = undefined

instance Game Bamp where

  name _ = "bamp"

  standard _ = Properties { players = 4, boardsize = 8, human = [True, False, False, False] }

  possible _ = PropertyRange { playersrange = [4], boardsizerange = [6, 8 .. 12] }
  
  new pr = let s = boardsize pr
               h = s `div` 2
           in Bamp $ array ((0, 0), (s - 1, s - 1))
              [((x, y), Empty) | x <- [0 .. s - 1], y <- [0 .. s - 1]]
              // concat [[((0    ,     i), Piece 0), ((    i, 0    ), Piece 0)] | i <- [0 .. h - 1]]
              // concat [[((0    , h + i), Piece 1), ((    i, s - 1), Piece 1)] | i <- [0 .. h - 1]]
              // concat [[((s - 1, h + i), Piece 2), ((h + i, s - 1), Piece 2)] | i <- [0 .. h - 1]]
              // concat [[((s - 1,     i), Piece 3), ((h + i, 0    ), Piece 3)] | i <- [0 .. h - 1]]
              // [((1    , 1    ), Ball    )]
{-{
              // [ ((0    , 0    ), Portal 0)
                 , ((0    , s - 1), Portal 1)
                 , ((s - 1, s - 1), Portal 2)
                 , ((s - 1, 0    ), Portal 3)
                 , ((1    , 1    ), Ball    )
                 ]
}-}  
  moves pr p (Bamp s) = map (move $ boardsize pr) (allMoves (boardsize pr) p s)

  showmove pr p (Bamp s) i = let (x, y) = allMoves (boardsize pr) p s !! i
                             in ['a' ..] !! x : show (boardsize pr - y)
  
  value pr p (Bamp s)
    | null $ moves pr p (Bamp s) = let bsz = boardsize pr
                                   in owner bsz (findBall s) |> 1 $ replicate 4 (-1)
{-{
                                   in case findBall s
                                      of (x, y) | (x, y) == (0      , 0      ) -> [ 1, -1, -1, -1]
                                                | (x, y) == (0      , bsz - 1) -> [-1,  1, -1, -1]
                                                | (x, y) == (bsz - 1, bsz - 1) -> [-1, -1,  1, -1]
                                                | (x, y) == (bsz - 1, 0      ) -> [-1, -1, -1,  1]
                                         _ -> [0, 0, 0, 0]
}-}
    | otherwise = [0, 0, 0, 0]

  board p pr vart _ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg <- varCreate $ grate rectZero 0 (0, 0) sizeZero

    let 
    
      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        return ()
        t <- varGet vart
        let Bamp st = state t
            bsz     = boardsize pr
        b <- border dc (bsz, bsz)
        let g       = grate r b (bsz, bsz) (Size 1 1)
        varSet varg g
        tileBitmap dc r marble
        for 0 (bsz - 1) (\i -> do
          drawTextRect dc [['A' ..] !! i]  $ edge g (i,  -1)
          drawTextRect dc [['A' ..] !! i]  $ edge g (i, bsz)
          drawTextRect dc (show (bsz - i)) $ edge g ( -1, i)
          drawTextRect dc (show (bsz - i)) $ edge g (bsz, i)
          )
        for 0 (bsz - 1) (\i -> for 0 (bsz - 1) (\j ->
          drawRect dc (field g (i, j)) [brushKind := BrushTransparent, penColor := setLum 0.2 $ colorplayer $ owner bsz (i, j)]
          ))
        for 0 (bsz - 1) (\i -> for 0 (bsz - 1) (\j ->
          drawField dc (field g (i, j)) (st ! (i, j))
          ))

      onclick :: Point -> IO ()
      onclick point = do 
        t <- varGet vart
        g <- varGet varg
        let Bamp st = state t
            n       = locate g point 
        case lookup n $ zip (allMoves (boardsize pr) (player t) st) [0..] of
          Nothing -> return ()
          Just  i -> move' i

    set p [ on click    := onclick
          , on paint    := onpaint
          , on resize  ::= repaint
          ]

owner :: Int -> (Int, Int) -> Player
owner bsz (i, j)
  | 2 * i <  bsz && 2 * j <  bsz = 0
  | 2 * i <  bsz && 2 * j >= bsz = 1
  | 2 * i >= bsz && 2 * j >= bsz = 2
  | 2 * i >= bsz && 2 * j <  bsz = 3
  | otherwise                    = error "owner: Unexpected value"

drawField :: DC () -> Rect -> BampField -> IO ()
drawField _  (Rect _ _ _ _) Empty      = return ()
drawField dc (Rect x y w h) (Portal p) = circle dc (pt (x + w `div` 2) (y + h `div` 2)) (2 * (min w h) `div` 5) [penColor := colorplayer p, penWidth := 2, brushKind := BrushTransparent]
drawField dc (Rect x y w h) (Piece  p) = drawRect dc (Rect (x + w `div` 10) (y + h `div` 10) (w - w `div` 5) (h - h `div` 5)) [brushColor := colorplayer p]
drawField dc (Rect x y w h) Ball       = circle dc (pt (x + w `div` 2) (y + h `div` 2)) (2 * (min w h) `div` 5) [brushColor := rgb 100 80 (40 :: Int)]

allMoves :: Int -> Player -> Array (Int, Int) BampField -> [(Int, Int)]
allMoves _bsz p s = filter valid $ filter (\i -> s ! i == Piece p) $ indices s
 where
  (a, b) = findBall s

  valid :: (Int, Int) -> Bool
  valid (x, y) | x /= a && y /= b = False
               | x < a = all (\i -> ok (i, y)) [x + 1 .. a + 1]
               | x > a = all (\i -> ok (i, y)) [a - 1 .. x - 1]
               | y < b = all (\j -> ok (x, j)) [y + 1 .. b + 1]
               | y > b = all (\j -> ok (x, j)) [b - 1 .. y - 1]
               | otherwise = error "allMoves: Unexpected value"

  ok :: (Int, Int) -> Bool
  ok t = inRange (bounds s) t
         && case s ! t of Piece _ -> False
                          _       -> True

move :: Int -> (Int, Int) -> (Player, Bamp) -> (Player, Bamp)
move bsz (x, y) (p, Bamp s) = let (a, b) = findBall s
                                  new'   = follow (a, b) (signum (a - x), signum (b - y))
                              in (owner bsz new', Bamp $ s // [((x, y), Empty), ((a, b), Piece p), (new', Ball)])
 where
  follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
  follow (x', y') (dx, dy)
    | not $ inRange (bounds s) (x' + dx, y' + dy) = (x', y')
    | s ! (x' + dx, y' + dy) == Empty             = follow (x' + dx, y' + dy) (dx, dy)
    | isPortal $ s ! (x' + dx, y' + dy)           = follow (x' + dx, y' + dy) (dx, dy)
    | otherwise                                   = (x', y')

findBall :: Array (Int, Int) BampField -> (Int, Int)
findBall s = case dropWhile (not.(== Ball).snd) $ assocs s
             of ((i, Ball):_) -> i
                []            -> (0, 0)
                _             -> error "findBall: Unexpected value"

colorplayer :: Int -> Color
colorplayer 0 = hsl 0.66 1   0.5
colorplayer 1 = hsl 0    1   0.5
colorplayer 2 = hsl 0.33 1   0.5
colorplayer 3 = hsl 0.82 1   0.5
colorplayer 4 = hsl 0.11 0.7 0.5
colorplayer 5 = hsl 0    0   0.5
colorplayer _ = hsl 0    0   0

