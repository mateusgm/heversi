{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.Routes where

import Data.Map                 (Map, (!))
import Happstack.Server         (ServerPart, Response)
import Control.Monad.Trans      (liftIO)

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






