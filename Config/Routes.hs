module System.Routes where

import System.Routes      (Route(GETs, GETl, POSTs))
import Controllers.Game    as Game
import Controllers.Home    as Home     


routes = [GETs  ""            Home.index ,
          POSTs "start"       Home.start ,
          GETs  "game"        Game.index ,
          POSTs "game/begin"  Game.begin ,
          GETs  "game/play"   Game.play  ,
          GETl  "game/get"    Game.get   ,
          GETl  "game/end"    Game.end   ]
--          GETs  "chat"        Chat.index ,
--          POSTl "chat/send"   Chat.send  ,
--          GETl  "chat/get"    Chat.get   ]
    

