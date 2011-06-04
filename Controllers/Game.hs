module Controllers.Game
   (create, play, update, get, index
   )where

import System.Routes
import System.Templates
import System.Cookies

import qualified Adaptors.User as User
import qualified Adaptors.Game as Game


index :: Controller
index m = do userID <- getCookie "user"
             user <- User.get userID
             logged <- User.getLogged
             let info = List "logged" logged 
                     <*> Map' "user" user
             liftIO $ render "Game/index" info


create :: Controller
create m = do userID <- getCookie "user"
              user <- User.get userID
              let opponent = read $ m!"opponent"
              opponent' <- User.get opponent
              game <- Game.create user opponent' 
              setCookie "game" $ Game.gID game
              liftIO $ render "" empty

play :: Controller
play m = do userID <- getCookie "user"
            gameID <- getCookie "game"
            user <- User.get userID
            game <- Game.get gameID
            let info = Map' "game" game
                    <*> List "board" (Game.board game)
                    <+> Map' "user" user
            liftIO $ render "Game/play" info

get :: Controller
get m = do gameID <- getCookie "game"
           game <- Game.get gameID
           let info = Map' "game" game
                   <*> List "board" (Game.board game)
                   <+> List "available" (Game.available game)
                   <+> Map' "state" (Game.gState game)
           liftIO $ render "Game/update" info

update :: Controller
update m = do gameID <- getCookie "game"
              game <- Game.get gameID
              user <- User.get . read $ m!"user"
              let move = ((read $ m!"y", read $ m!"x"), read $ m!"player")
              game' <- Game.play game user move                        
              let info = Map' "game" game'
                      <*> List "board" (Game.board game')
                      <+> List "available" (Game.available game')
                      <+> Map' "state" (Game.gState game')
              liftIO $ render "Game/update" info

check :: Controller
create m = do userID <- getCookie "user"
              user <- User.get userID
              let opponent = read $ m!"opponent"
              opponent' <- User.get opponent
              game <- Game.create user opponent' 
              setCookie "game" $ Game.gID game
              liftIO $ render "" empty

-- debug -- liftIO . putStrLn $ (show user) ++ " " ++ (show m)

