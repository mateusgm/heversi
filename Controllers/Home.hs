module Controllers.Home where

import System.Routes
import System.Templates
import System.Cookies

import Models.User


index :: Controller
index m = liftIO $ render "Home/index" . (<!>) $ Pack m "url"

start :: Controller
start m = do user <- addUser $ m!"name"
             logged <- getLogged
             setCookie "userID" $ id user
             let info = Pack logged "logged" 
                     <*> Pack user "user"
             liftIO $ render "Home/start" info
