module Controllers.Home        (index, start)
  where

import System.Types            (Controller, Attribute(..))
import System.Templates        (render, render')
import Models.User

import Data.Map                (insert)
import Control.Monad.Trans     (liftIO)
import Happstack.Server        (CookieLife(..), mkCookie,
                                addCookie, lookCookieValue,
                                readCookieValue)


index :: Controller
index m = liftIO $ render "Home/index" m

start :: Controller
start m = do let m' = insert "users" (List logged) m
             liftIO $ render' "Home/start" m'
