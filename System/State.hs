{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.State
  where

import Happstack.State    (Component(..), End, Proxy(..), (:+:), mkMethods,
                           startSystemState, createCheckpoint, shutdownSystem)
import System.Types       (AppState(..))
import Adaptors.Game

-- Change this type definition

type Entities = GameDir :+: End



-- Automagic

state = startSystemState a
  where a = Proxy :: Proxy AppState
  
checkAndShut c = do createCheckpoint c
                    shutdownSystem c

instance Component AppState where
  type Dependencies AppState = Entities 
  initialValue = AppState

$(mkMethods ''AppState [])
