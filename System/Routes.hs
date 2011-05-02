module Engine.Routes (routes) where

import Engine.Types         (Route(GETs, GETl, POSTs))
import Controllers.Game     as Game
import Controllers.Home     as Home     



routes = [GETs  ""            Home.index ,
          POSTs ""            Home.start ,
          GETs  "game"        Game.index ,
          GETs  "game/begin"  Game.begin ,
          GETs  "game/play"   Game.play  ,
          GETl  "game/get"    Game.get   ,
          GETl  "game/end"    Game.end   ,
          GETs  "chat"        Chat.index ,
          POSTl "chat/send"   Chat.send  ,
          GETl  "chat/get"    Chat.get   ,
    

