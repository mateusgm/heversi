module Models.User             (addUser, getUser, getLogged)
  where
  
import qualified Adaptors.User as U    
import Happstack.State         (query, update)
import Happstack.Server        (ServerPart)  
import Models.Types            (User(..))
import Data.Map                (Map, insert, singleton)
import Control.Monad.Trans     (liftIO)

addUser :: String -> ServerPart User
addUser name = do user <- update $ U.AddUser name
                  return user

getUser :: Int -> ServerPart User
getUser id = do user <- query $ U.GetUser id
                return user

toMap :: User -> Map String String
toMap (Human n i) = insert "name" n . singleton "id" . show $ i

getLogged :: [User]
getLogged =  [("2","abcdef")   ,
              ("20", "qwerty") ,
              ("5", "uololou") ,
              ("11", "dev3")   ,
              ("13", "alauaca")]
          
