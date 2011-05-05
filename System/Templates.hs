module System.Templates          (render, render')         
  where

import System.Types              (HtmlString(HtmlString))
import Happstack.Server          (Response, toResponse)
import qualified
  Text.StringTemplate as HST     (render)
import Text.StringTemplate       (STGroup, StringTemplate, toString,
                                  directoryGroup, addSubGroup,
                                  getStringTemplate)


_root = "Views/"
_includes = _root ++ "Includes/"


render :: String -> String -> IO Response
render s t = do
  inc <- directoryGroup _includes :: IO (STGroup String)
  grp <- directoryGroup $ _root ++ s :: IO (STGroup String)
  let grp' = addSubGroup grp inc
  let Just t' = getStringTemplate t grp'
  return . toResponse . HtmlString . HST.render $ t' 

render' :: String -> String -> IO Response
render' s t = do
  g <- directoryGroup $ _root :: IO (STGroup String)
  let Just t' = getStringTemplate t g
  return . toResponse . HtmlString . (++ s) . toString $ t' 
