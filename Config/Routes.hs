module Config.Routes where

import System.Routes      (Route(GETs, GETl, POSTs))
import Controllers.Game    as Game
import Controllers.Home    as Home     


routes = [GETs  ""            Home.index ,
          POSTs "start"       Home.start ,
          POSTs "game/create" Game.create,
          GETs  "game/play"   Game.play  ,
          POSTs "game/update" Game.update]
    

