module System.Templates          (render)         
  where

import System.Types              (HtmlString(HtmlString))
import Happstack.Server          (Response, toResponse)
import qualified
  Text.StringTemplate as HST     (render)
import Text.StringTemplate       (STGroup, StringTemplate,
                                  directoryGroup, getStringTemplate)


_viewsDir = "Views/"


render :: String -> String -> IO Response
render s t = do
  g <- directoryGroup $ _viewsDir ++ s:: IO (STGroup String)
  let Just t' = getStringTemplate t g
  return . toResponse . HtmlString . HST.render $ t' 

