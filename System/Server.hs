module System.Server        (server)
  where

import System.Types         (Route(..), Matching(..), Controller)
import System.Routes        (routes)
import Debug.Trace
import Data.Map             (Map, fromList, union, singleton)
import Control.Monad        (msum)
import Happstack.Server     (Response, ServerPart, Method(..), Browsing(..),
                             nullConf, simpleHTTP, serveDirectory,
                             ok, dirs, path, methodM, methodOnly,
                             decodeBody, defaultBodyPolicy, lookPairs)

                          
-- ======================= main functions =======================

server _ = simpleHTTP nullConf handlers
  where handlers = msum $ (map routeHandler routes) ++ public 
        public = [serveDirectory DisableBrowsing [] "Resources"]

routeHandler :: Route -> ServerPart Response
routeHandler (GETs "" c) =          createHandler GET Strict c
routeHandler (GETs  p c) = dirs p $ createHandler GET Strict c
routeHandler (GETl  p c) = dirs p $ createHandler GET Loose c
routeHandler (POSTs p c) = dirs p $ createHandler POST Strict c

createHandler :: Method -> Matching -> Controller -> ServerPart Response
createHandler m z c = do matchMethod z m
                         decodeRequest m
                         pr <- lookPairs
                         if (z == Strict) then strictHandler pr
                          else path $ looseHandler pr
   where strictHandler = c . parsePairs
         looseHandler pr = c . union (parsePairs pr) . singleton "url"
                     
                    
-- ====================== auxiliary functions ===================

matchMethod :: Matching -> Method -> ServerPart () 
matchMethod Strict = methodM
matchMethod Loose  = methodOnly

decodeRequest :: Method -> ServerPart ()
decodeRequest POST = decodeBody $ defaultBodyPolicy "/tmp/" 4096 4096 4096
decodeRequest GET  = return ()  
                       
parsePairs :: [(String, Either FilePath String)] -> Map String String
parsePairs = fromList . map extractPair
  where extractPair (s1, Right s2) = (s1, s2)

