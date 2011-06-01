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
index m = liftIO . render "Game/index" . (<!>) $ Map' "url" m 

begin :: Controller
begin m = do userID <- getCookie "userID"
             user <- getUser userID
             opponent <- getUser . read $ m!"opponent"
             game <- Game.begin user opponent
             setCookie "gameID" $ gID game
             let info = Map' "board" (board game)  
                     <*> Map' "state" (state game) 
                     <+> Map' "user" user      
             liftIO $ render "Game/board" info

play :: Controller
play m  = liftIO . render "Game/index" . (<!>) $ Map' "url" m 

get :: Controller
get m   = liftIO . render "Game/index" . (<!>) $ Map' "url" m 

end :: Controller
end m   = liftIO . render "Game/index" . (<!>) $ Map' "url" m 

-- liftIO . putStrLn $ (show user) ++ " " ++ (show m)
