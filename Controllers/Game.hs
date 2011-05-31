module Controllers.Game           (index, begin, play, get, end)
  where

import System.Types               (Controller, Attribute(..))
import System.Templates           (render, render')
import Control.Monad.Trans        (liftIO)
import Data.Map                   ((!), insert, singleton)
import Happstack.Server           (CookieLife(..), mkCookie,
                                   addCookie, lookCookieValue,
                                   readCookieValue)
import Models.User                (addUser, getUser)

index :: Controller
index m = liftIO . render "Game/index" . singleton "url" . Multi $ m

begin :: Controller
begin m = do userID <- readCookieValue "userID"
             user <- getUser userID
            -- let a = insert "board" (List board)
            --       . insert "state" (Multi state)
             let a = singleton "user" (Multi user)
             liftIO $ render "Game/board" a 

play :: Controller
play m  = liftIO . render "Game/index" . singleton "url" . Multi $ m

get :: Controller
get m   = liftIO . render "Game/index" . singleton "url" . Multi $ m

end :: Controller
end m   = liftIO . render "Game/index" . singleton "url" . Multi $ m
