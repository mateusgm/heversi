module Controllers.Home     (index, start)
  where

import Happstack.Server.RqData (RqData, checkRq, getDataFn)
import Happstack.Server     (ok, toResponse, lookCookieValue, readCookieValue,
                             addCookie, CookieLife(Session), mkCookie)
import System.Types         (Controller)
import System.Templates     (render)
import Control.Monad.Trans  (liftIO)
import Control.Monad.Error
import Control.Exception    (try)
import Debug.Trace

index :: Controller
index _ = do addCookie Session (mkCookie "requests" "0")
             liftIO $ render "" "myFavoriteAnimalBase"

start :: Controller
start m = ok . toResponse $ "<html><p>oioioi</p></html>"


