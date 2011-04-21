module Reversi
where

data Player = None | IA | Human
              deriving (Show)

type Board  = [[Player]]
type Coord  = (Int, Int)
type Move   = (Player, Coord)  

boardInit :: Int -> [[Player]]
boardInit n = replicate n . replicate n None

elemIndices

extractPiece :: Board
extractPiece [[a1,b2,b3,b4, (x,y)

makeMove :: Board -> Move -> Board

