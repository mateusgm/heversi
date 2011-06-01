module Controllers.Game
   (index, begin, play, get, end
   )where

import System.Routes
import System.Templates
import System.Cookies

import Models.User
import qualified Models.Game as Game (begin)
import Models.Game hiding (begin)


index :: Controller
index m = liftIO . render "Game/index" . (<!>) $ Pack m "url"

begin :: Controller
begin m = do userID <- getCookie "userID"
             user <- getUser userID
             opponent <- getUser . read $ m!"opponent"
             game <- Game.begin user opponent
             setCookie "gameID" $ gID game
             let info = Pack (board game) "board" 
                     <*> Pack (state game) "state"
                     <+> Pack user "user"      
             liftIO $ render "Game/board" info

play :: Controller
play m  = liftIO . render "Game/index" . (<!>) $ Pack m "url"

get :: Controller
get m   = liftIO . render "Game/index" . (<!>) $ Pack m "url"

end :: Controller
end m   = liftIO . render "Game/index" . (<!>) $ Pack m "url"

-- liftIO . putStrLn $ (show user) ++ " " ++ (show m)
