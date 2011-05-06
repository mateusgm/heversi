module Controllers.Home        (index, start)
  where

import System.Types            (Controller, Attribute(..))
import System.Templates        (render, insert, singleton)
import Models.User             (addUser, getLogged)

import Data.Map                ((!))
import Control.Monad.Trans     (liftIO)
import Happstack.Server        (CookieLife(..), mkCookie,
                                addCookie, lookCookieValue,
                                readCookieValue)


index :: Controller
index m = liftIO $ render "Home/index" m

start :: Controller
start m = do let name = m!"name"
             user <- addUser name
             logged <- getLogged
             let a = insert "logged" logged $ singleton "user" user
             liftIO $ render "Home/start" a
