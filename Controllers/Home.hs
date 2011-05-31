module Controllers.Home        (index, start)
  where

import System.Types            (Controller, Attribute(..))
import System.Templates        (render)
import Models.User             (addUser, getLogged)

import Data.Map                ((!), insert, singleton)
import Control.Monad.Trans     (liftIO)
import Happstack.Server        (CookieLife(..), mkCookie,
                                addCookie, lookCookieValue,
                                readCookieValue)


index :: Controller
index m = liftIO $ render "Home/index" . singleton "url" . Multi $  m

start :: Controller
start m = do user <- addUser $ m!"name"
             logged <- getLogged
             addCookie Session . mkCookie "userID" $ id user
             let params = List logged "logged" 
                       <+> Index user "user"
             liftIO $ render "Home/start" params
