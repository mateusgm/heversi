{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.State 
  where

import Happstack.State    (Component(..), End, (:+:), mkMethods)
import System.Types
import Adaptors.Game

-- Change this type definition

type Entities = GameDir :+: End


-- Automagic

instance Component AppState where
  type Dependencies AppState = Entities 
  initialValue = AppState

$(mkMethods ''AppState [])
