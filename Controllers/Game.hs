module Controllers.Game
   (create, play, update, get, index, ai
   )where

import System.Routes
import System.Templates
import System.Cookies

import Models.User
import qualified Models.Game as Game (create, play, playAI)
import Models.Game hiding (create, play, playAI)


index :: Controller
index m = do userID <- getCookie "user"
             user <- getUser userID
             logged <- getLogged
             let info = List "logged" logged 
                     <*> Map' "user" user
             liftIO $ render "Game/index" info


create :: Controller
create m = do userID <- getCookie "user"
              user <- getUser userID
              let opponent = read $ m!"opponent"
              opponent' <- getUser opponent
              game <- Game.create user opponent' 
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
                      <+> List "available" (available game')
                      <+> Map' "state" (gState game')
              liftIO $ render "Game/update" info

ai :: Controller
ai m = do gameID <- getCookie "game"
          game <- getGame gameID
          game' <- Game.playAI game                       
          let info = Map' "game" game'
                  <*> List "board" (board game')
                  <+> List "available" (available game')
                  <+> Map' "state" (gState game')
          liftIO $ render "Game/update" info

-- debug -- liftIO . putStrLn $ (show user) ++ " " ++ (show m)

