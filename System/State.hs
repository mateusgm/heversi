{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.State where

import Models.Repo

import Data.Data          (Data, Typeable)
import Happstack.Server   (ServerPart) 
import Happstack.State


-- the construction

type Entities = UserRepo :+: GameRepo :+: End


-- data

data AppState = AppState
                deriving (Data, Typeable)


-- api

state = startSystemState a
  where a = Proxy :: Proxy AppState
  
checkAndShut c = do createCheckpoint c
                    shutdownSystem c


-- automagic

instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = Entities 
  initialValue = AppState

$(mkMethods ''AppState [])
