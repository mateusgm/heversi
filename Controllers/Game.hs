module Controllers.Game   (index, begin, play, get, end)
  where

import System.Types               (Controller, Attribute(..))
import System.Templates           (render, render')
import Control.Monad.Trans        (liftIO)
import Data.Map                    ((!), insert, singleton)

index :: Controller
index m = liftIO . render "Game/index" . singleton "url" . Multi $ m

begin :: Controller
begin m = liftIO . render "Game/index" . singleton "url" . Multi $ m

play :: Controller
play m  = liftIO . render "Game/index" . singleton "url" . Multi $ m

get :: Controller
get m   = liftIO . render "Game/index" . singleton "url" . Multi $ m

end :: Controller
end m   = liftIO . render "Game/index" . singleton "url" . Multi $ m
