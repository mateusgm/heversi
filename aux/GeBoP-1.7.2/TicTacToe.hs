
---------------
-- TicTacToe --
---------------

module TicTacToe (TicTacToe, tictactoe) where

import Game
import Graphics.UI.WX     hiding (border, point)
import Graphics.UI.WXCore hiding (point)
import Tools

data TicTacToe = TicTacToe [Maybe Player] deriving (Eq, Show)

tictactoe :: TicTacToe
tictactoe = undefined

instance Game TicTacToe where

  name _ = "tictactoe"
{-
rules _ =    "Tictactoe"
          ++ "\n"
          ++ "\nTictactoe is played on a 3x3 board."
          ++ "\nTwo players put their pieces in empty squares in turns."
          ++ "\nThe first player to make a row of three pieces wins."
  information _ =    "Tictactoe is one of the easiest and best known strategic boardgames"
                ++ "\nin the world. You will probably find it in every book or program on"
                ++ "\ngame theory, and this one is no exception."
-}
  standard _ = Properties { players = 2, boardsize = 3, human = [True, False] }

  possible _ = PropertyRange { playersrange = [2], boardsizerange = [3] }
  
  new _p = TicTacToe $ replicate 9 Nothing
  
  moves _ _ (TicTacToe s)
    | any (\p -> any (tttwinning p s) tttrows) [0, 1] = []
    | otherwise                                       = map (move . snd) $ possiblemoves s

  showmove _pr _p (TicTacToe s) i = let j = snd (possiblemoves s !! i)
                                    in ["abc" !! (j `mod` 3), "321" !! (j `div` 3)]

  value _ _ (TicTacToe s) | any (tttwinning 0 s) tttrows = [ 1, -1]
                          | any (tttwinning 1 s) tttrows = [-1,  1]
                          | otherwise                    = [ 0,  0]

  board p _pr vart _ia move' = do

    marble <- bitmapCreateLoad "images\\marble.bmp" wxBITMAP_TYPE_ANY
    varg <- varCreate $ grate rectZero 0 (0, 0) sizeZero

    let 

      onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        t <- varGet vart
        let TicTacToe st = state t
        b <- border dc (3, 3)
        let g = grate r b (3, 3) (Size 1 1)
        varSet varg g
        tileBitmap dc r marble
        for 0 2 (\i -> do
          drawTextRect dc ["ABC" !! i]   $ edge g (i, -1)
          drawTextRect dc ["ABC" !! i]   $ edge g (i,  3)
          drawTextRect dc (show (3 - i)) $ edge g (-1, i)
          drawTextRect dc (show (3 - i)) $ edge g ( 3, i)
          )
        drawGrate dc g [brushKind := BrushTransparent]
        for 0 2 (\i -> for 0 2 (\j ->
          case st !! (i + 3 * j) of Just 0  -> drawCross  dc $ field g (i, j)
                                    Just 1  -> drawCircle dc $ field g (i, j)
                                    Nothing -> return ()
                                    _       -> error "Unexpected value"
          ))
         
      onclick :: Point -> IO ()
      onclick point = do 
        t <- varGet vart
        g <- varGet varg
        let TicTacToe st = state t
            (i, j)       = locate g point 
            n | i < 0 || i >= 3 || j < 0 || j >= 3 = -1
              | otherwise = (i + 3 * j)
        case lookup n (zip (map snd $ possiblemoves st) [0..]) of
          Nothing -> return ()
          Just n'  -> move' n'

    set p [ on click := onclick
          , on paint := onpaint
          ]
          
    return ()

possiblemoves :: [Maybe Player] -> [(Maybe Player, Int)]
possiblemoves st = filter ((== Nothing) . fst) $ zip st [0 .. 8]

drawCross :: DC () -> Rect -> IO ()
drawCross dc (Rect x y w h) =
  do line dc (pt (x + w `div` 10) (y + h `div` 10)) (pt (x + w - w `div` 10) (y + h - h `div` 10)) [penColor := blue, penWidth := 2]
     line dc (pt (x + w `div` 10) (y + h - h `div` 10)) (pt (x + w - w `div` 10) (y + h `div` 10)) [penColor := blue, penWidth := 2]

drawCircle :: DC () -> Rect -> IO ()
drawCircle dc (Rect x y w h) =
  do circle dc (pt (x + w `div` 2) (y + h `div` 2)) (2 * (min w h) `div` 5) [penColor := red, penWidth := 2, brushKind := BrushTransparent]

move :: Int -> (Player, TicTacToe) -> (Player, TicTacToe)
move n (p, TicTacToe s) = (1 - p, TicTacToe $ (n |> Just p) s)

tttrows :: [[Int]]
tttrows = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]]

tttwinning :: Player -> [Maybe Player] -> [Int] -> Bool
tttwinning p s is = all (== Just p) $ map (s !!) is
