module Config.Routes (routes, Route) where


import Happstack.Server  (Method(GET, POST))
import Controllers.Game  (game)
--, chat, home)

type Matcher = String
type Controller = String -> String

type Route = (Matcher, Method, Controller)


routes = [("game", GET, game),
          ("", GET, game)]
--,
--          ("chat", chat),
--          ("home", home)];




