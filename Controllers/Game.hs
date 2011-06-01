module Controllers.Game           (index, begin, play, get, end)
  where

import System.Routes
import System.Templates
import System.Cookies

import Models.User
import Models.Game


index :: Controller
index m = liftIO . render "Game/index" . (<!>) $ Pack m "url"

begin :: Controller
begin m = do userID <- getCookie "userID"
             user <- getUser userID
             oponnent <- getUser $ m!"opponent"
             game <- Game.begin user opponent
             setCookie "gameID" $ id game
             let params = Pack (board game) "board" 
                       <+> Pack (state game) "state"
                       <*> Pack user "user"      
             liftIO $ render "Game/board" a

play :: Controller
play m  = liftIO . render "Game/index" . (<!>) $ Pack m "url"

get :: Controller
get m   = liftIO . render "Game/index" . (<!>) $ Pack m "url"

end :: Controller
end m   = liftIO . render "Game/index" . (<!>) $ Pack m "url"
