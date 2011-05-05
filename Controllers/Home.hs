module Controllers.Home           (index, start)
  where

import System.Types               (Controller)
import System.Templates           (render, render')

import Control.Monad.Trans        (liftIO)
import Happstack.Server           (ok, toResponse, lookCookieValue, 
                                   readCookieValue, addCookie,
                                   CookieLife(Session), mkCookie)


index :: Controller
index _ = do addCookie Session (mkCookie "requests" "0")
             liftIO $ render "" "myFavoriteAnimalBase"

start :: Controller
start m = ok . toResponse $ "<html><p>oioioi</p></html>"


