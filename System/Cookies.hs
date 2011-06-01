module System.Cookies where

import Happstack.Server        (CookieLife(..), mkCookie,
                                addCookie, lookCookieValue, HasRqData,
                                readCookieValue, ServerPart, Response)

setCookie :: Show a => String -> a -> ServerPart ()
setCookie key value = addCookie life . mkCookie key . show $ value 
   where life = Session

getCookie :: Read a => String -> ServerPart a
getCookie = readCookieValue

