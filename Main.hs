module Main where

import System.Server        (server)
import System.State         (state, checkAndShut)
import Control.Exception    (bracket)

main :: IO ()
main = startApp 

startApp :: IO ()
startApp = bracket state checkAndShut server    
     
