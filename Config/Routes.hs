module Config.Routes (Route(GETs, GETl, POSTs),
                      routes, Controller,
                      Matching(Strict, Loose)) where

import Data.Map                   (Map)
import Controllers.Game as Game   (index, send, home)

-- ======================================================

data Matching   = Strict | Loose
type Path       = String
type Controller = Map String String -> String

-- the 's' refers to strict matching with subpath
-- the 'p' refers to strict matching
-- the 'l' refers to loose matching
data Route      = GETs  Path Controller  | 
                  GETl  Path Controller  | 
                  POSTs Path Controller       

-- ======================================================

routes = [GETs "game"      Game.index,
          GETl "game/send" Game.send ,
          GETs ""          Game.home ]    

