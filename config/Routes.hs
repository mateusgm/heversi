module Routes (routes) where

import Happstack.Server (Method(GET, POST))
import Controllers      (game, chat, home)


routes = [("game", game),
          ("chat", chat),
          ("home", home)];




