module System.Templates             (render)         
  where

import Happstack.Server             (Response, toResponse)
import Happstack.Server.SimpleHTTP  (ToMessage(..))
import Data.ByteString.Char8        (pack)         
import Data.ByteString.Lazy.UTF8    (fromString)
import qualified
  Text.StringTemplate as HST        (render)
import Text.StringTemplate          (STGroup, StringTemplate,
                                     directoryGroup, getStringTemplate)

_viewsDir = "Views/"

render :: String -> String -> IO Response
render s t = do
  g <- directoryGroup $ _viewsDir ++ s:: IO (STGroup String)
  let Just t' = getStringTemplate t g
  return . toResponse . HtmlString . HST.render $ t' 

newtype HtmlString = HtmlString String
instance ToMessage HtmlString where
  toContentType _ = pack "text/html;charset=utf-8"
  toMessage (HtmlString s) = fromString s
