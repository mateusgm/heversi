{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Adaptors.User
  where
  
import Models.Types           (User(..))
import Data.Map               (Map, size, insert, (!), empty)
import Data.Data              (Data, Typeable)
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)
import Happstack.State        (Component(..), Version, End, Query,
                               Update, deriveSerialize, mkMethods)

-- data type

newtype UserRepo = UserRepo (Map Int User)
                   deriving (Typeable, Data)

-- the operations

addUser :: String -> Update UserRepo User
addUser n = do UserRepo r <- get
               let u = User n $ (size r) + 1
               put . UserRepo . insert id u $ r
               return u

getUser :: Int -> Query UserRepo User
getUser id = do UserRepo dir <- ask
                return $ dir!id

-- the automagic

instance Version UserRepo
$(deriveSerialize ''UserRepo)

instance Component UserRepo where
  type Dependencies UserRepo = End
  initialValue = UserRepo empty  

$(mkMethods ''UserRepo ['addUser, 'getUser])

