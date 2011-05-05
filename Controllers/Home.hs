module Controllers.Home           (index, start)
  where

import System.Types               (Controller)
import System.Templates           (render, render')

import Control.Monad.Trans        (liftIO)
import Happstack.Server           (CookieLife(..), mkCookie,
                                   addCookie, lookCookieValue,
                                   readCookieValue)


index :: Controller
index m = liftIO $ render "Home/index" m

start :: Controller
start m = liftIO $ render' "Home/start" m

