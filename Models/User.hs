module Models.User             (addUser, getUser, getLogged)
  where
  
import qualified Adaptors.User as U    
import Happstack.State         (query, update)
import Happstack.Server        (ServerPart)  
import Models.Types            (User(..))
import Data.Map                (Map, insert, singleton)
import Control.Monad.Trans     (liftIO)

addUser :: String -> ServerPart (Map String String)
addUser name = do user <- update $ U.AddUser name
                  return $ toMap user

getUser :: Int -> ServerPart (Map String String)
getUser id = do user <- query $ U.GetUser id
                return $ toMap user
                
toMap :: User -> Map String String
toMap (Human n i) = insert "name" n . singleton "id" . show $ i
                
getLogged :: [(String, String)]
getLogged =  [("2","abcdef")   ,
              ("20", "qwerty") ,
              ("5", "uololou") ,
              ("11", "dev3")   ,
              ("13", "alauaca")]
          
