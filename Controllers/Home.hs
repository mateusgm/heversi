module Controllers.Home           (index, start)
  where

import System.Types               (Controller)
import System.Templates           (render, render')
import Adaptors.Game             
import Models.Types

import qualified Models.Game.Engine as T

import Control.Monad.Trans        (liftIO)
import Happstack.Server           (ok, toResponse, lookCookieValue, 
                                   readCookieValue, addCookie,
                                   CookieLife(Session), mkCookie)
import Happstack.State            (query, update)

import Debug.Trace

index :: Controller
index _ = do addCookie Session (mkCookie "requests" "0")
             id1 <- update . AddGame . Game T.start Android $ Android
             liftIO $ render' (show id1) "myFavoriteAnimalBase"

start :: Controller
start m = ok . toResponse $ "<html><p>oioioi</p></html>"


