{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies, TypeOperators #-}

module System.State
  where

import System.Types       (AppState(..))
import Happstack.State    (Component(..), End, Proxy(..), (:+:),
                           mkMethods, startSystemState,
                           shutdownSystem, createCheckpoint, )

-- change here

import Adaptors.User
import Adaptors.Game

type Entities = UserRepo :+: GameRepo :+: End


-- api

state = startSystemState a
  where a = Proxy :: Proxy AppState
  
checkAndShut c = do createCheckpoint c
                    shutdownSystem c

-- automagic

instance Component AppState where
  type Dependencies AppState = Entities 
  initialValue = AppState

$(mkMethods ''AppState [])
