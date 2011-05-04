{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Models.Game where

import Models.Game.Engine     (GameState(..), play, start)
import Models.Types           (Game(..), GameDir(..), User(..))

import Data.Map               (Map, empty, size, insert, (!))
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)
import Happstack.State        (Proxy(..), Query, Update,
                               mkMethods, query, update)


-- ============= Database operations ============== --


addGame :: Game -> Update GameDir Int
addGame g = do GameDir dir <- get
               let id = (size dir) + 1
               put . GameDir . insert id g $ dir
               return id

getGame :: Int -> Query GameDir Game
getGame id = do GameDir dir <- ask
                return $ dir!id

$(mkMethods ''GameDir ['addGame, 'getGame])





