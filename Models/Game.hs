{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Models.Game where

import Models.Game.Engine  (GameState, play, start)
import Models.Types        (Game(..), GameDir(..))

import Data.Data           (Data, Typeable)
import Data.Map            (Map, empty, lookup)
import Happstack.State     (Proxy(..), Query, Update,
                            createCheckpoint, deriveSerialize,
                            mkMethods, query, update,
                            startSystemState, shutdownSystem)


-- ============= Database operations ============== --


addGame :: Game -> Update GameDir Int
addGame g = do GameDir dir <- get
               let id = (size dir) + 1
               put . GameDir . insert id g $ id
               return id

getGame :: Int -> Query GameDir (Maybe Game)
getGame id = do GameDir dir <- ask
                return . lookup dir $ id

$(mkMethods ''GameDir ['addGame, 'getGame])

