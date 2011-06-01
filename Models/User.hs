module Models.User where
  
import Models.Type
import Models.Repo.User
import System.State


addUser :: String -> ServerPart User
addUser name = do user <- update $ AddUser name
                  return user

getUser :: Int -> ServerPart User
getUser id = do user <- query $ GetUser id
                return user

          
