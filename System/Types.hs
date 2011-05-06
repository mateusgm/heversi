{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.Types
  where

import qualified Prelude as P       (map)
import Prelude                       hiding (map)
import Data.Map                     (Map, map, fromList)
import Happstack.Server             (ServerPart, Response)
import Happstack.Server.SimpleHTTP  (ToMessage(..))
import Data.ByteString.Char8        (pack)         
import Data.ByteString.Lazy.UTF8    (fromString)
import Data.Data                    (Data, Typeable)
import Happstack.State              (Version, deriveSerialize)
import Text.StringTemplate          (ToSElem(..))
import Text.StringTemplate.Classes  

-- ==================    Routes     ================== --

data Matching       = Strict | Loose
                      deriving (Eq)
type Path           = String
type Controller     = Map String Attribute -> ServerPart Response

-- the 's' refers to strict matching
-- the 'l' refers to loose matching
data Route          = GETs  Path Controller  | 
                      GETl  Path Controller  | 
                      POSTs Path Controller  |
                      POSTl Path Controller


-- ==================   Template    ================== --
                      
newtype HtmlString = HtmlString String
instance ToMessage HtmlString where
  toContentType _ = pack "text/html;charset=utf-8"
  toMessage (HtmlString s) = fromString s

data Attribute = Multi  (Map String String) |
                 List   [(String,String)]   |
                 Simple (String)

instance ToSElem Attribute where
  toSElem (Multi a)  = toSElem a
  toSElem (List a)   = toSElem a 
  toSElem (Simple a) = toSElem a 

instance Show Attribute where
  show (Multi a)  = show a
  show (Simple a) = a
  show (List a) = show a

-- ==================     State     ================== --

data AppState = AppState
                deriving (Data, Typeable)

instance Version AppState
$(deriveSerialize ''AppState)

