module System.Templates          (render, render')         
  where

import System.Types              (HtmlString(HtmlString), Attribute)

import Data.Map                  (Map, toList)
import Data.List                 (findIndices, splitAt)
import Happstack.Server          (Response, toResponse)
import qualified
  Text.StringTemplate as HST     (render)
import Text.StringTemplate          (ToSElem(..))  
import Text.StringTemplate       (STGroup, StringTemplate, toString,
                                  directoryGroup, addSubGroup,
                                  getStringTemplate, setAttribute,
                                  setManyAttrib)

-- filepaths

_root = "Views/"
_includes = _root ++ "Includes/"


-- exported functions

render :: String -> Map String Attribute -> IO Response
render p m = do t <- template p m
                return . toResponse . HtmlString . HST.render $ t 

render' :: String -> Map String Attribute -> IO Response
render' p m = do t <- template p m
                 return . toResponse . HtmlString . (++ (show m)) 
                  . toString $ t 

-- auxiliary functions

template :: String -> Map String Attribute -> IO (StringTemplate String)
template p m = do let (d,t) = path p
                  i <- directoryGroup _includes
                  g <- directoryGroup $ _root ++ d
                  let g' = addSubGroup g i
                      Just t' = getStringTemplate t g'
                  return . setManyAttrib (toList m) $ t'

path :: String -> (String, String)
path p = splitAt i p
  where i = if (null ix) then (0) else (1 + (last ix)) 
        ix = findIndices (== '/') $ p

