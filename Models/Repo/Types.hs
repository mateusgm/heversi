{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}
module Models.Repo.Types
   (module Data.Data,
    module Data.Map,
    module Happstack.State,
    module Control.Monad.Reader,
    module Control.Monad.State,
    module Models.User,
    module Models.Game,
    module Models.Repo.Types
   )where 

import Models.Game
import Models.User

import Data.Data              (Data, Typeable)
import Data.Map               (Map, size, insert, (!), empty,
                               toList, update, member)
import Happstack.State        (Component(..), End, Version, Query,
                               Update, deriveSerialize, mkMethods)
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)


instance Version Stone
$(deriveSerialize ''Stone)

instance Version GameState
$(deriveSerialize ''GameState)

instance Version User
$(deriveSerialize ''User)

instance Version Game
$(deriveSerialize ''Game)

