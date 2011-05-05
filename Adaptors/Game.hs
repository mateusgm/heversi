{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Adaptors.Game
  where
  
import Models.Types           (Game)
import Data.Map               (Map, size, insert, (!), empty)
import Data.Data              (Data, Typeable)
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)
import Happstack.State        (Component(..), Version, End, Query,
                               Update, deriveSerialize, mkMethods)

-- data type

newtype GameDir = GameDir (Map Int Game)
                  deriving (Typeable, Data)

-- the operations

addGame :: Game -> Update GameDir Int
addGame g = do GameDir dir <- get
               let id = (size dir) + 1
               put . GameDir . insert id g $ dir
               return id

getGame :: Int -> Query GameDir Game
getGame id = do GameDir dir <- ask
                return $ dir!id

-- the automagic

instance Version GameDir
$(deriveSerialize ''GameDir)

instance Component GameDir where
  type Dependencies GameDir = End
  initialValue = GameDir empty  

$(mkMethods ''GameDir ['addGame, 'getGame])

