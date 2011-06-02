module System.Templates
   (Info(..), Infoable(..), render, render', (<+>), (<*>), (<!>),
    module Data.Map
   ) where

import Config.Templates

import Data.Map                  (Map, toList, insert, singleton, empty)
import Data.List                 (findIndices, splitAt)
import Happstack.Server          (ToMessage(..), Response, toResponse)
import Data.ByteString.Char8     (pack)         
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified
  Text.StringTemplate as HST     (render)
import Text.StringTemplate       (STGroup, StringTemplate, toString,
                                  directoryGroup, addSubGroup,
                                  getStringTemplate, setAttribute,
                                  setManyAttrib, ToSElem(..))

-- types

newtype HtmlString = HtmlString String
instance ToMessage HtmlString where
  toContentType _ = pack "text/html;charset=utf-8"
  toMessage (HtmlString s) = fromString s

   
data Info a = Map' String a  |
              List String [a]

data Info' = Map''  (Map String String) |
             List'  [(Map String String)] |
             Simple String

instance ToSElem Info' where
  toSElem (Map'' a)  = toSElem a
  toSElem (List' a)   = toSElem a 
  toSElem (Simple a) = toSElem a 

instance Show Info' where
  show (Map'' a)  = show a
  show (List' a) = show a
  show (Simple a) = a
  

class Infoable a where
   toMap :: a -> Map String String
   toAttr :: a -> Info'
   toAttrX :: [a] -> Info'
   toAttr = Map'' . toMap 
   toAttrX = List' . map toMap

instance Infoable (String) where
   toMap s = empty
   toAttr s = Simple s
   toAttrX s = List' []

instance Infoable (Map String String) where
   toMap m = m


(<*>) :: (Infoable a, Infoable b) => Info a -> Info b -> Map String Info'
i1 <*> i2 = (<!>) i2 <+> i1 

(<+>) :: Infoable a => Map String Info' -> Info a -> Map String Info'
map <+> (Map' key value) = insert key (toAttr value) map
map <+> (List key value) = insert key (toAttrX value) map

(<!>) :: Infoable a => Info a -> Map String Info'
(<!>) (Map' key value) = singleton key $ toAttr value 
(<!>) (List key value) = singleton key $ toAttrX value

-- exported functions

render :: String -> Map String Info' -> IO Response
render "" _ = return . toResponse $ ""
render p m = do t <- template p m
                return . toResponse . HtmlString . HST.render $ t 

render' :: String -> Map String Info' -> IO Response
render' p m = do t <- template p m
                 return . toResponse . HtmlString . (++ (show m)) 
                  . toString $ t 

-- auxiliary functions

template :: String -> Map String Info' -> IO (StringTemplate String)
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

