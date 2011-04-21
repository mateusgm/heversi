
---------
-- NIM --
---------

module Nim (Nim, nim) where

import Game
import Graphics.UI.WX
-- import Graphics.UI.WXCore
import Tools

data Nim = Nim Int deriving (Show, Eq)

nim :: Nim
nim = undefined

instance Game Nim where

  name _ = "nim"

  standard _ = Properties { players = 2, boardsize = 21, human = [True, False, False] }
  
  possible _ = PropertyRange { playersrange = [2, 3], boardsizerange = [1 .. 100] }
  
  new pr = Nim $ boardsize pr
  
  moves pr _ (Nim n) = map (move pr) $ filter (<= n) [1 .. 3]

  showmove _ _ _ i = numberword (i + 1)

  value pr p (Nim n)
    | n == 0    = (prev p |> 1) $ replicate (players pr) (negate 1)
    | otherwise = replicate (players pr) 0
    where 
      prev :: Player -> Player
      prev p' = (p' - 1) `mod` players pr

  board p _pr vart _ move' = do

    st <- staticText p [ text := "There are currently quite a number of rods.\n" ]
    b1 <- button p []
    b2 <- button p []
    b3 <- button p []

    let 

      onpaint _dc _r = do
        t <- varGet vart
        let Nim n = state t
        set st [ text := "There are currently " ++ numberword n ++ " rods.\n"
                      ++ "How many will you take away?" ]

    set b1 [ text := "one rod"   , on command := move' 0 ]
    set b2 [ text := "two rods"  , on command := move' 1 ]
    set b3 [ text := "three rods", on command := move' 2 ]

    set p [ layout   := floatCentre $ column 4 [ centre $ widget st
                                               , row 4 [widget b1, widget b2, widget b3] 
                                               ]
          , on paint := onpaint
          ]
          
    return ()

{-
dist :: Int -> Int -> Int
dist x y = let i = x * x + y * y
               f = fromInteger $ toInteger i
               s = sqrt f
           in floor s

drawButton :: Int -> DC () -> Rect -> IO ()
drawButton n dc (Rect x y w h) =
  do circle dc (pt (x + w `div` 2) (y + h `div` 2)) (min w h `div` 2) [brushKind := BrushTransparent]
     let t = w `div` (n + 1)
     for 1 n (\i -> line dc (pt (x + i * t) (y + h `div` 5)) (pt (x + i * t) (y + h - h `div` 5)) [])
-}

move :: Properties -> Int -> (Player, Nim) -> (Player, Nim)
move pr n (p, Nim m) = ((p + 1) `mod` players pr, Nim (m - n))
