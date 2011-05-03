module System.Types        (Route(GETs, GETl, POSTs), Controller,
                            Matching(Strict, Loose))
  where

import Happstack.Server    (ServerPart, Response)
import Data.Map            (Map)

data Matching       = Strict | Loose
                      deriving (Eq)
type Path           = String
type Controller     = Map String String -> ServerPart Response


-- the 's' refers to strict matching
-- the 'l' refers to loose matching
data Route          = GETs  Path Controller  | 
                      GETl  Path Controller  | 
                      POSTs Path Controller  |
                      POSTl Path Controller
