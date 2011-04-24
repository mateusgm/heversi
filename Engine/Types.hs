module Engine.Types (Route(GETs, GETl, POSTs), Controller,
                     Matching(Strict, Loose)) where


data Matching   = Strict | Loose
type Path       = String
type Controller = Map String String -> String


-- the 's' refers to strict matching
-- the 'l' refers to loose matching
data Route      = GETs  Path Controller  | 
                  GETl  Path Controller  | 
                  POSTs Path Controller  
