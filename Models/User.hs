module Models.User
   (module Models.Types,
    module Models.User
   )where
  
import System.State
import System.Templates

import Models.Types        (User(..), uID)
import Models.Repo.User

addUser :: String -> ServerPart User
addUser name = do user <- update $ AddUser name
                  return user

getUser :: Int -> ServerPart User
getUser id
   | id == 0   = return Android
   | otherwise = do user <- query $ GetUser id
                    return user

getLogged :: ServerPart [User]
getLogged = do users <- query $ GetUsers 
               return users

instance Infoable User where
   toMap (Human name id) = insert "name" name
                         . singleton "id" $ show id
          
