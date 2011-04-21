
----------
-- MAIN --
----------

module Main where

import Game
import Graphics.UI.WX
import GUI

import Ataxx
import Bamp
import Halma
import Hex
import Kram
import Nim
import Reversi
import TicTacToe
import Zenix

import System.Directory   (setCurrentDirectory)
import Paths_GeBoP        (getDataDir)

games :: [GeneralGame]
games = [ Game ataxx
        , Game bamp
        , Game halma
        , Game hex
        , Game kram
        , Game nim
        , Game reversi
        , Game tictactoe
        , Game zenix
        ]

main :: IO ()
main =
  getDataDir          >>=
  setCurrentDirectory >> 
  start (gui games)

exit :: IO ()
exit = putStrLn "When will you learn you have to use :q?"
