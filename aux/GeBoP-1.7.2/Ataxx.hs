
-------------
-- Ataxx --
-------------

module Ataxx (Ataxx, ataxx) where

import Game
import Array
-- import Graphics.UI.WX
import Graphics.UI.WX     hiding (border, point)
import Graphics.UI.WXCore hiding (point)
import Tools

data Ataxx = Ataxx (Array (Int, Int) (Maybe Player)) deriving (Eq, Show)

type AtaxxMove = Maybe ((Int, Int), (Int, Int))

ataxx :: Ataxx
ataxx = undefined

instance Game Ataxx where

  name _ = "ataxx"

  standard _ = Properties { players = 2, boardsize = 7, human = [True, False] }
  
  possible _ = PropertyRange { playersrange = [2], boardsizerange = [5 .. 9] }
  
  new pr = let s = boardsize pr
           in Ataxx $ array ((0, 0), (s - 1, s - 1))
              [((x, y), Nothing) | x <- [0 .. s - 1], y <- [0 .. s - 1]]
              // [ ((0    , 0    ), Just 0)
                 , ((0    , s - 1), Just 1)
                 , ((s - 1, 0    ), Just 1)
                 , ((s - 1, s - 1), Just 0)
                 ]
  
  moves pr p (Ataxx s) = map (move $ boardsize pr) (allMoves (boardsize pr) p s)

  showmove pr p (Ataxx s) i = case allMoves (boardsize pr) p s !! i
                              of Nothing -> "skip turn"
                                 Just ((x1, y1), (x2, y2)) -> "abcdefghi" !! x1 : show (boardsize pr - y1)
                                                    ++ "-" ++ "abcdefghi" !! x2 : show (boardsize pr - y2)
  
  value pr p (Ataxx s) 
    | null $ moves pr p (Ataxx s) = (\i -> [i, -i]) $ (fromInteger . toInteger) $ signum $ count $ elems s
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
    vare <- varCreate (Nothing :: Maybe (Int, Int))

    let 
    
      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        t <- varGet vart
        e <- varGet vare
        let Ataxx st = state t
            bsz      = boardsize pr
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
        case e of
          Nothing     -> return ()
          Just (i, j) -> drawBrightPiece dc $ field g (i, j)
        if human pr !! player t && allMoves (boardsize pr) (player t) st == [Nothing]
          then wait p 1 $ do
            when ia $ infoDialog p "You can't move!" "You have to skip this turn, since there are no possible moves."
            move' 0
          else return ()

      onclick :: Point -> IO ()
      onclick point = do 
        t <- varGet vart
        g <- varGet varg
        let Ataxx st = state t
            n        = locate g point 
        when (inRange (bounds st) n) $ when (st ! n == Just (player t)) $ do
          varSet vare $ Just n
          repaint p

      onunclick :: Point -> IO ()
      onunclick point = do 
        t <- varGet vart
        g <- varGet varg
        e <- varGet vare
        case e of
          Nothing -> return ()
          Just m  -> do
            let Ataxx st = state t
                n        = locate g point 
            varSet vare Nothing
            repaint p
            case lookup (Just (m, n)) $ zip (allMoves (boardsize pr) (player t) st) [0..] of
              Nothing -> return ()
              Just  i -> move' i

    set p [ on click    := onclick
          , on unclick  := onunclick
          , on paint    := onpaint
          , on resize  ::= repaint
          ]

allMoves :: Int -> Player -> Array (Int, Int) (Maybe Player) -> [AtaxxMove]
allMoves _bsz p s
    | (null $ valid p s) && (not $ null $ valid (1 - p) s) = [Nothing]
    | dead (1 - p) (elems s)                               = []
    | otherwise                                            = map (Just) $ valid p s
    where
      valid :: Player -> Array (Int, Int) (Maybe Player) -> [((Int, Int), (Int, Int))]
      valid p' s' = concatMap complete $ filter ((== Just p') . (s' !)) $ indices s'
        where
          complete :: (Int, Int) -> [((Int, Int), (Int, Int))]
          complete xy = zip (repeat xy) (from xy)

          from :: (Int, Int) -> [(Int, Int)]
          from xy = filter (flip check Nothing) $ map (xy +-) $ grows ++ jumps

          check :: (Int, Int) -> Maybe Player -> Bool
          check m f | not $ inRange (bounds s) m = False
                    | otherwise                  = s ! m == f

      dead :: Player -> [Maybe Player] -> Bool
      dead p' (Just q  : fs) = p' /= q && dead p' fs
      dead p' (Nothing : fs) =           dead p' fs
      dead _  []             = True

move :: Int -> AtaxxMove -> (Player, Ataxx) -> (Player, Ataxx)
move _bsz (Just (f, t)) (p, Ataxx s) = (1 - p, Ataxx $ s // (phase1 ++ phase2))
  where 
    phase1 :: [((Int, Int), Maybe Player)]
    phase1 | t `elem` map (+- f) jumps = [(f, Nothing), (t, Just p)]
           | otherwise                 = [              (t, Just p)]

    phase2 :: [((Int, Int), Maybe Player)]
    phase2 = zip (filter (flip check $ Just $ 1 - p) $ map (+- t) grows) $ repeat $ Just p

    check :: (Int, Int) -> Maybe Player -> Bool
    check m f' | not $ inRange (bounds s) m = False
               | otherwise                  = s ! m == f'
move _ Nothing (p, rs) = (1 - p, rs)

grows :: [(Int, Int)]
grows = concat [[(x, 1), (-x, -1), (-1, x), (1, -x)] | x <- [ 0 .. 1]]

jumps :: [(Int, Int)]
jumps = concat [[(x, 2), (-x, -2), (-2, x), (2, -x)] | x <- [-1 .. 2]]

(+-) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) +- (c, d) = (a + c, b + d)

drawPiece :: Player -> DC () -> Rect -> IO ()
drawPiece p dc (Rect x y w h) = do
  case p of 0 -> set dc [brushColor := rgb 32  96 (192 :: Int)]
            1 -> set dc [brushColor := rgb 192 96 (32  :: Int)]
            _ -> set dc [brushColor := white]
  circle dc (pt (x + w `div` 2) (y + h `div` 2)) (2 * (min w h) `div` 5) []

drawBrightPiece :: DC () -> Rect -> IO ()
drawBrightPiece dc (Rect x y w h) = do
  set dc [penWidth := 2, penColor := yellow, brushKind := BrushTransparent]
  circle dc (pt (x + w `div` 2) (y + h `div` 2)) (2 * (min w h) `div` 5) []

