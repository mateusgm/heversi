
----------
-- Kram --
----------

module Kram (Kram, kram) where

import Game
import Array
import Graphics.UI.WX     hiding (border, point)
import Graphics.UI.WXCore hiding (point)
import Tools

data Kram = Kram (Array (Int, Int) (Either Player Bool)) deriving (Eq, Show)

kram :: Kram
kram = undefined

instance Game Kram where

  name _ = "kram"

  standard _ = Properties { players = 2, boardsize = 5, human = [True, False] }

  possible _ = PropertyRange { playersrange = [2], boardsizerange = [4 .. 7] }
  
  new pr = let s = boardsize pr
           in Kram $ array ((0, 0), (s - 1, s - 1)) [((x, y), Right False) | x <- [0 .. s - 1], y <- [0 .. s - 1]]
  
  moves pr p (Kram s) = map (move $ boardsize pr) (allMoves (boardsize pr) p s)

  showmove pr p (Kram s) i = case allMoves (boardsize pr) p s !! i
                             of Nothing -> "skip turn"
                                Just (x, y) -> sm (x, y) ++ ":" ++ case p
                                                                   of 0 -> sm (x + 1, y)
                                                                      1 -> sm (x, y + 1)
                                                                      _ -> "error"
    where
      sm :: (Int, Int) -> String
      sm (x, y) = "abcdefghij" !! x : show (boardsize pr - y)
  
  value pr p (Kram s) 
    | null $ moves pr p (Kram s) = (\i -> [i, -i]) $ (fromInteger . toInteger) $ signum $ count $ elems s
    | otherwise = (\i -> let f = (fromInteger . toInteger) i / (fromInteger . toInteger) (sqr $ boardsize pr) in [f, -f]) $ open + count (elems s)
    where
      open :: Int
      open = length (moves pr 0 (Kram s)) - length (moves pr 1 (Kram s))
      count :: [Either Player Bool] -> Int
      count []             =  0
      count (Right _ : fs) =      count fs
      count (Left 0  : fs) =  1 + count fs
      count (Left 1  : fs) = -1 + count fs
      count _              = error "count: Unexpected value"

  board p pr vart ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg <- varCreate $ grate rectZero 0 (0, 0) sizeZero

    let 
    
      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        t <- varGet vart
        let Kram st = state t
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
        drawGrate dc g [brushKind := BrushTransparent]
        for 0 (bsz - 1) (\i -> for 0 (bsz - 1) (\j ->
          case st ! (i, j) of Left p' -> drawPiece p' dc (field g (i, j))
                              Right _ -> return ()
          ))
        if human pr !! player t && allMoves (boardsize pr) (player t) st == [Nothing]
          then wait p 1 $ do
            when ia $ infoDialog p "You can't move!" "You have to skip this turn, since there are no possible moves."
            move' 0
          else return ()

      onclick :: Point -> IO ()
      onclick point = do 
        t <- varGet vart
        g <- varGet varg
        let Kram st = state t
            n       = Just $ locate g point
        case lookup n $ zip (allMoves (boardsize pr) (player t) st) [0..] of
          Nothing -> return ()
          Just  i -> move' i

    set p [ on click    := onclick
          , on paint    := onpaint
          , on resize  ::= repaint
          ]

drawPiece :: Player -> DC () -> Rect -> IO ()
drawPiece 0 dc (Rect x y w h) = do
  set dc [brushColor := rgb 96 16 (255 :: Int) ]
  drawRect dc (Rect (x + w `div` 10) (y + h `div` 10) (2 * w - w `div` 5) (h - h `div` 5)) []
drawPiece 1 dc (Rect x y w h) = do
  set dc [brushColor := rgb 192 64 (16 :: Int) ]
  drawRect dc (Rect (x + w `div` 10) (y + h `div` 10) (w - w `div` 5) (2 * h - h `div` 5)) []
drawPiece _ _  _              = error "drawPiece: unexpected value"

allMoves :: Int -> Player -> Array (Int, Int) (Either Player Bool) -> [Maybe (Int, Int)]
allMoves bsz p s 
  | (null $ valid p s) && (not $ null $ valid (1 - p) s) = [Nothing]
  | otherwise                                            = map Just $ valid p s
  where
    valid :: Player -> Array (Int, Int) (Either Player Bool) -> [(Int, Int)]
    valid p' s' = filter mag $ indices s'
      where
        mag :: (Int, Int) -> Bool
        mag (x, y) | p' == 0 = x < bsz - 1 && s' ! (x, y) == Right False && s' ! (x + 1, y) == Right False 
        mag (x, y) | p' == 1 = y < bsz - 1 && s' ! (x, y) == Right False && s' ! (x, y + 1) == Right False 
        mag _                = error "mag: Unexpected value"

move :: Int -> Maybe (Int, Int) -> (Player, Kram) -> (Player, Kram)
move _bsz (Just (x, y)) (0, Kram s) = (1, Kram $ s // [((x, y), Left 0), ((x + 1, y), Right True)])
move _bsz (Just (x, y)) (1, Kram s) = (0, Kram $ s // [((x, y), Left 1), ((x, y + 1), Right True)])
move _ Nothing (p, ks) = (1 - p, ks)
move _ _       _       = error "move: Unexpected value"
