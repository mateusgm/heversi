module Main where

import Config.Routes    (routes, Route)

import Control.Monad    (msum)
import Happstack.Server (nullConf, simpleHTTP, Response, ServerPart,
                         ok, dir, toResponse, uriRest,
                         methodM, methodOnly,
                         Browsing(DisableBrowsing), serveDirectory,
                         decodeBody, defaultBodyPolicy)

main :: IO ()
main = simpleHTTP nullConf $ appRoutes

appRoutes :: ServerPart Response
appRoutes = 
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
       msum $ map createRoute routes ++
            [serveDirectory DisableBrowsing ["index.html"] "public"]

-- path :: (FromReqURI a, MonadPlus m, ServerMonad m) => (a -> m b) -> m b

createRoute :: Route -> ServerPart Response
createRoute ("",b,c) = do methodM b
                          ok $ toResponse $ c ""        
createRoute (a,b,c) = dir a $ uriRest (\s -> do methodOnly b
                                             ok $ toResponse $ c s)
