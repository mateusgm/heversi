module Main where

import Config.Routes    (routes, Route)

import Data.Either      (rights)
import Text.Regex       (splitRegex, mkRegex, subRegex)
import Control.Monad    (msum)
import Happstack.Server (nullConf, simpleHTTP, Response, ServerPart,
                         ok, dir, toResponse, uriRest,
                         methodM, methodOnly, lookPairs,
                         Browsing(DisableBrowsing), serveDirectory,
                         decodeBody, defaultBodyPolicy)

main :: IO ()
main = simpleHTTP nullConf $ appRoutes

appRoutes :: ServerPart Response
appRoutes = 
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
       msum $ map createRoute routes ++
            [serveDirectory DisableBrowsing ["index.html"] "public"]

createRoute :: Route -> ServerPart Response
createRoute ("",b,c)  = do methodM b
                           ok $ toResponse $ c [] []       
createRoute (a,b,c)   = dir a $ uriRest $ parser
  where parser s      = do methodOnly b
                           p <- lookPairs 
                           let p1 = unzip p
                               p2 = zip (fst p1) (rights $ snd p1)
                               r1 = mkRegex "[?](.*)"
                               r2 = mkRegex "/"
                               s1 = tail s
                               s2 = splitRegex r2 $ subRegex r1 s1 ""
                           ok $ toResponse $ c s2 p2
                      
