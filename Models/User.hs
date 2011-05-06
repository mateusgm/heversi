module Models.User     (addUser, getLogged)
  where
  
import Adaptors.User    (
import Happstack.State  (query, update)  
import Models.Types     (User(..))


addUser :: String -> IO User
addUser   = query  . AddUser

getLogged :: IO ([(String, String)])
getLogged = liftIO $ [("2","abcdef")   ,
                      ("20", "qwerty") ,
                      ("5", "uololou") ,
                      ("11", "dev3")   ,
                      ("13", "alauaca")]
          
