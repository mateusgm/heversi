module System.Cookies


import Happstack.Server        (CookieLife(..), mkCookie,
                                addCookie, lookCookieValue,
                                readCookieValue)

setCookie :: Show a => String -> a -> ServerPart Response
setCookie key value = addCookie life . mkCookie key . show $ value 
   where life = Session

getCookie = readCookieValue

