module Controllers.Home           (index, start)
  where

import System.Types               (Controller)
import System.Templates           (render, render')

import Control.Monad.Trans        (liftIO)
import Happstack.Server           (CookieLife(..), mkCookie, addCookie,
                                   lookCookieValue, readCookieValue)


index :: Controller
index _ = do addCookie Session (mkCookie "requests" "0")
             liftIO $ render "Home" "index"

start :: Controller
start m = liftIO $ render "Home" "start"


