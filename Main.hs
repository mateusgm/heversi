module Main where

import Config.Routes (routes)

import Control.Monad    (msum)
import Happstack.Server (nullConf, simpleHTTP, 
                         dir, path, methodM)

main :: IO ()
main = simpleHTTP nullConf $ routes

routes :: ServerPart Response
routes = 
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
       msum [ route a b | (a,b) <- routes ] ++
            [serveDirectory EnableBrowsing ["index.html"] "public"]

route :: String -> (String -> Response) -> ServerPartT Response         
route a b = dir a $ path (\s -> b s)
