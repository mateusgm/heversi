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
start m = do let name = m!"name"
             user <- addUser name
             let logged = getLogged
                 a = insert "logged" (List logged)
                     . singleton "user" . Multi $ user
             liftIO $ render "Home/start" a
