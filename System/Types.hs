{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.Types
  where

import Data.Map                     (Map)
import Happstack.Server             (ServerPart, Response)
import Happstack.Server.SimpleHTTP  (ToMessage(..))
import Data.ByteString.Char8        (pack)         
import Data.ByteString.Lazy.UTF8    (fromString)
import Data.Data                    (Data, Typeable)
import Happstack.State              (Version, deriveSerialize)


-- ==================    Routes     ================== --

data Matching       = Strict | Loose
                      deriving (Eq)
type Path           = String
type Controller     = Map String String -> ServerPart Response

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


-- ==================     State     ================== --

data AppState = AppState
                deriving (Data, Typeable)

instance Version AppState
$(deriveSerialize ''AppState)

