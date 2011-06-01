module System.Templates
   (Info(..), Infoable(..), render, render', (<+>), (<*>), (<!>),
    module Data.Map
   ) where

import Config.Templates

import Data.Map                  (Map, toList, insert, singleton)
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

data Attribute = Multi  (Map String String) |
                 List'   [(String,String)]   |
                 Simple (String)

instance ToSElem Attribute where
  toSElem (Multi a)  = toSElem a
  toSElem (List' a)   = toSElem a 
  toSElem (Simple a) = toSElem a 

instance Show Attribute where
  show (Multi a)  = show a
  show (Simple a) = a
  show (List' a) = show a

class Infoable a where
   toMap :: a -> Map String String
   toAttr :: a -> Attribute
   toAttrX :: [a] -> Attribute
   toAttr = Multi . toMap 
   toAttrX = List' . concatMap (toList . toMap)

instance Infoable (Map String String) where
   toMap m = m 

data Info a = Pack a String |
              List [a] String

(<*>) :: Infoable a => Info a -> Info a -> Map String Attribute
i1 <*> i2 = (<!>) i2 <+> i1 

(<+>) :: Infoable a => Map String Attribute -> Info a -> Map String Attribute
map <+> (Pack value key) = insert key (toAttr value) map
map <+> (List value key) = insert key (toAttrX value) map

(<!>) :: Infoable a => Info a -> Map String Attribute
(<!>) (Pack value key) = singleton key $ toAttr value 
(<!>) (List value key) = singleton key $ toAttrX value

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

