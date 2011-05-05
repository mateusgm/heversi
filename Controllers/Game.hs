module Controllers.Game   (index, begin, play, get, end)
  where

import System.Types               (Controller)
import System.Templates           (render, render')
import Control.Monad.Trans        (liftIO)


index :: Controller
index m = liftIO $ render "Game/index" m

begin :: Controller
begin m = liftIO $ render "Game/begin" m

play :: Controller
play m  = liftIO $ render "Game/play" m

get :: Controller
get m   = liftIO $ render "Game/get" m

end :: Controller
end m   = liftIO $ render "Game/end" m
