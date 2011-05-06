module Models.User        (addUser, getLogged)
  where
  
import qualified Adaptors.User as U    
import Happstack.State    (query, update)
import Happstack.Server   (ServerPart)  
import Models.Types       (User(..))
import Data.Map           (Map, insert, singleton)
import Control.Monad.Trans     (liftIO)

addUser :: String -> ServerPart (Map String String)
addUser n = do (Human n i) <- update  . U.AddUser $ n
               return . insert "name" n . singleton "id" . show $ i
                
getLogged :: [(String, String)]
getLogged =  [("2","abcdef")   ,
              ("20", "qwerty") ,
              ("5", "uololou") ,
              ("11", "dev3")   ,
              ("13", "alauaca")]
          
