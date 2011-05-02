module Engine.Routes (routes) where

import Engine.Types         (Route(GETs, GETl, POSTs))
import Controllers.Game     as Game   



routes = [GETs ""          Game.home ,
          GETl "game/send" Game.send ,
          GETs "all"       Game.index]    

