
-------------
-- Reversi --
-------------

module Reversi (Reversi, reversi) where

import Game
import Array
-- import Graphics.UI.WX
import Graphics.UI.WX     hiding (border)
import Graphics.UI.WXCore
import Tools

data Reversi = Reversi (Array (Int, Int) (Maybe Player)) deriving (Eq, Show)

reversi :: Reversi
reversi = undefined

instance Game Reversi where

  name _ = "reversi"

  standard _ = Properties { players = 2, boardsize = 8, human = [True, False] }

  possible _ = PropertyRange { playersrange = [2], boardsizerange = [6, 8, 10] }

  new pr = let s = boardsize pr
               h = s `div` 2
           in Reversi $ array ((0, 0), (s - 1, s - 1))
              [((x, y), Nothing) | x <- [0 .. s - 1], y <- [0 .. s - 1]]
              // [ ((h - 1, h - 1), Just 0)
                 , ((h    , h - 1), Just 1)
                 , ((h - 1, h    ), Just 1)
                 , ((h    , h    ), Just 0)
                 ]
  
  moves pr p (Reversi s) = map (move $ boardsize pr) (allMoves (boardsize pr) p s)

  showmove pr p (Reversi s) i = case allMoves (boardsize pr) p s !! i
                                of Nothing -> "skip turn"
                                   Just (x, y) -> "abcdefghij" !! x : show (boardsize pr - y)
  
  value pr p (Reversi s) 
    | null $ moves pr p (Reversi s) = (\i -> [i, -i]) $ (fromInteger . toInteger) $ signum $ count $ elems s
    | otherwise = (\i -> let f = (fromInteger . toInteger) i / (fromInteger . toInteger) (sqr $ boardsize pr) in [f, -f]) $ count $ elems s
    where
      count :: [Maybe Player] -> Int
      count []             =  0
      count (Nothing : fs) =      count fs
      count (Just 0  : fs) =  1 + count fs
      count (Just 1  : fs) = -1 + count fs
      count _              = error "value: Unexpected value"

  board p pr vart ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg <- varCreate $ grate rectZero 0 (0, 0) sizeZero

    let 
    
      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        t <- varGet vart
        let Reversi st = state t
            bsz        = boardsize pr
        b <- border dc (bsz, bsz)
        let g = grate r b (bsz, bsz) (Size 1 1)
        varSet varg g
        tileBitmap dc r marble
        for 0 (bsz - 1) (\i -> do
          drawTextRect dc [['A' ..] !! i]  $ edge g (i,  -1)
          drawTextRect dc [['A' ..] !! i]  $ edge g (i, bsz)
          drawTextRect dc (show (bsz - i)) $ edge g ( -1, i)
          drawTextRect dc (show (bsz - i)) $ edge g (bsz, i)
          )
        drawGrate dc g [brushKind := BrushTransparent]
        for 0 (bsz - 1) (\i -> for 0 (bsz - 1) (\j ->
          case st ! (i, j) of Just p' -> drawPiece p' dc $ field g (i, j)
                              Nothing -> return ()
          ))
        if human pr !! player t && allMoves (boardsize pr) (player t) st == [Nothing]
          then wait p 1 $ do
            when ia $ infoDialog p "You can't move!" "You have to skip this turn, since there are no possible moves."
            move' 0
          else return ()

      onclick :: Point -> IO ()
      onclick point' = do 
        t <- varGet vart
        g <- varGet varg
        let Reversi st = state t
            n          = Just $ locate g point' 	
        case lookup n $ zip (allMoves (boardsize pr) (player t) st) [0..] of
          Nothing -> return ()
          Just  i -> move' i

    set p [ on click    := onclick
          , on paint    := onpaint
          , on resize  ::= repaint
          ]

drawPiece :: Player -> DC () -> Rect -> IO ()
drawPiece p dc (Rect x y w h) = do
  case p of 0 -> set dc [brushColor := rgb 96  16 (255 :: Int) ]
            1 -> set dc [brushColor := rgb 192 64 (16 :: Int)  ]
            _ -> set dc [brushColor := white]
  circle dc (pt (x + w `div` 2) (y + h `div` 2)) (2 * (min w h) `div` 5) []

(+-) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) +- (c, d) = (a + c, b + d)

allMoves :: Int -> Player -> Array (Int, Int) (Maybe Player) -> [Maybe (Int, Int)]
allMoves _bsz p s 
  | (null $ valid p s) && (not $ null $ valid (1 - p) s) = [Nothing]
  | otherwise                                              = map Just $ valid p s
  where
    valid :: Player -> Array (Int, Int) (Maybe Player) -> [(Int, Int)]
    valid p' s' = filter (\xy -> or $ map (scan xy) dirs) . filter ((== Nothing) . (s' !)) $ indices s' 
      where
        dirs :: [(Int, Int)]
        dirs = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]
        scan  :: (Int, Int) -> (Int, Int) -> Bool
        scan  xy dxy =  check (xy +- dxy) (Just $ 1 - p') && scan1 (xy +- dxy) dxy
        scan1 :: (Int, Int) -> (Int, Int) -> Bool
        scan1 xy dxy =  check (xy +- dxy) (Just       p')
                    || (check (xy +- dxy) (Just $ 1 - p') && scan1 (xy +- dxy) dxy)
        check :: (Int, Int) -> Maybe Player -> Bool
        check m f | not $ inRange (bounds s') m = False
                  | otherwise                   = s' ! m == f

move :: Int -> Maybe (Int, Int) -> (Player, Reversi) -> (Player, Reversi)
move _bsz (Just m) (p, Reversi s) = (1 - p, Reversi $ s // ((m, Just p) : concatMap (scan m) dirs))
  where
    dirs :: [(Int, Int)]
    dirs = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]
    scan  :: (Int, Int) -> (Int, Int) -> [((Int, Int), Maybe Player)]
    scan  xy dxy | check (xy +- dxy) (Just $ 1 - p) = scan1 (xy +- dxy) dxy [(xy +- dxy, Just p)]
                 | otherwise                          = []
    scan1 :: (Int, Int) -> (Int, Int) -> [((Int, Int), Maybe Player)] -> [((Int, Int), Maybe Player)]
    scan1 xy dxy cs | check (xy +- dxy) (Just       p) = cs
                    | check (xy +- dxy) (Just $ 1 - p) = scan1 (xy +- dxy) dxy $ (xy +- dxy, Just p) : cs
                    | otherwise                          = []
    check :: (Int, Int) -> Maybe Player -> Bool
    check m' f | not $ inRange (bounds s) m' = False
               | otherwise                  = s ! m' == f
move _ Nothing (p, rs) = (1 - p, rs)
