module Engine.Routes (routes) where

import Engine.Types         (Route(GETs, GETl, POSTs))
import Controllers.Game     as Game   



routes = [GETs "game"      Game.index,
          GETl "game/send" Game.send ,
          GETs ""          Game.home ]    
