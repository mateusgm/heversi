{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.State     
  where

import Data.Data          (Data, Typeable)
import Happstack.State    (Component(..), End, Version, (:+:),
                           deriveSerialize, mkMethods)
                           
import Models.Game        


data AppState = AppState
                deriving (Data, Typeable)

instance Version AppState
$(deriveSerialize ''AppState)


instance Component AppState where
  type Dependencies AppState = GameDir :+: End
  initialValue = AppState

$(mkMethods ''AppState [])

