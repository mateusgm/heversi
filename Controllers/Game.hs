module Controllers.Game
   (create, play, update, get, index
   )where

import System.Routes
import System.Templates
import System.Cookies

import Models.User
import qualified Models.Game as Game (create, play)
import Models.Game hiding (create, play)


index :: Controller
index m = do userID <- getCookie "user"
             user <- getUser userID
             logged <- getLogged
             let info = List "logged" logged 
                     <*> Map' "user" user
             liftIO $ render "Home/start" info


create :: Controller
create m = do userID <- getCookie "user"
              user <- getUser userID
              opponent <- getUser . read $ m!"opponent"
              game <- Game.create user opponent
              setCookie "game" $ gID game
              liftIO $ render "" empty

play :: Controller
play m = do userID <- getCookie "user"
            gameID <- getCookie "game"
            user <- getUser userID
            game <- getGame gameID
            let game' = apply game user
                info = Map' "game" game'
                    <*> List "board" (board game)
                    <+> Map' "user" user
            liftIO $ render "Game/play" info

get :: Controller
get m = do userID <- getCookie "user"
           gameID <- getCookie "game"
           user <- getUser userID
           game <- getGame gameID
           let game' = apply game user
               info = Map' "game" game'
                   <*> List "board" (board game)
                   <+> Map' "user" user
                   <+> List "available" (available game)
                   <+> Map' "state" (gState game)
           liftIO $ render "Game/update" info

update :: Controller
update m = do userID <- getCookie "user"
              gameID <- getCookie "game"
              user <- getUser userID
              game <- getGame gameID
              let move = ((read $ m!"y", read $ m!"x"),read $ m!"player")
              game' <- Game.play game user move                        
              let game'' = apply game' user
                  info = Map' "game" game''
                      <*> List "board" (board game')
                      <+> Map' "user" user
                      <+> List "available" (available game')
                      <+> Map' "state" (gState game')
              liftIO $ render "Game/update" info


-- debug -- liftIO . putStrLn $ (show user) ++ " " ++ (show m)
