module Adaptors.User
   (module Models.User,
    module Adaptors.User
   )where
  
import System.State

import Models.User
import Models.Repo.User

add :: String -> ServerPart User
add name = do user <- update $ AddUser name
              return user

get :: Int -> ServerPart User
get id
   | id == 0   = return android
   | otherwise = do user <- query $ GetUser id
                    return user

getLogged :: ServerPart [User]
getLogged = do users <- query $ GetUsers 
               return users
          
