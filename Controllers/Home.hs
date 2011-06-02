module Controllers.Home where

import System.Routes
import System.Templates
import System.Cookies

import Models.User


index :: Controller
index m = liftIO $ render "Home/index" empty

start :: Controller
start m = do user <- addUser $ m!"name"
             logged <- getLogged
             setCookie "user" $ uID user
             liftIO $ render "" empty
