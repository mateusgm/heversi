{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Adaptors.Game
  where
  
import Models.Types           (Game)
import Models.Game.Engine     (start)
import Data.Map               (Map, size, insert, (!), empty)
import Data.Data              (Data, Typeable)
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)
import Happstack.State        (Component(..), Version, End, Query,
                               Update, deriveSerialize, mkMethods)

-- data type

newtype GameRepo = GameRepo (Map Int Game)
                   deriving (Typeable, Data)

-- the operations

addGame :: User -> User -> Update GameRepo Game
addGame b w = do UserRepo r <- get
                 let id = (size r) + 1
                     g = Game start b w id
                 put . GameRepo . insert id g $ r
                 return g

getGame :: Int -> Query GameRepo Game
getGame id = do GameRepo dir <- ask
                return $ dir!id

-- the automagic

instance Version GameRepo
$(deriveSerialize ''GameRepo)

instance Component GameRepo where
  type Dependencies GameRepo = End
  initialValue = GameRepo empty  

$(mkMethods ''GameRepo ['addGame, 'getGame])

