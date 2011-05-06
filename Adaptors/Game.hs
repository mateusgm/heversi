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

newtype GameRepo = GameRepo (Map Int Game)
                   deriving (Typeable, Data)

-- the operations

addGame :: Game -> Update GameRepo Int
addGame g = do GameRepo dir <- get
               let id = (size dir) + 1
               put . GameRepo . insert id g $ dir
               return id

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

