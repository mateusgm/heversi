{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}

module Models.Repo.User where
  
import Models.Types

import Data.Data              (Data, Typeable)
import Data.Map               (size, insert, (!), empty, toList)
import Control.Monad.Reader   (ask)
import Control.Monad.State    (get, put)


-- data type

newtype UserRepo = UserRepo (Map Int User)
                   deriving (Typeable, Data)

-- the operations

addUser :: String -> Update UserRepo User
addUser n = do UserRepo r <- get
               let id = (size r) + 1
                   u = Human n id
               put . UserRepo . insert id u $ r
               return u

getUser :: Int -> Query UserRepo User
getUser id = do UserRepo dir <- ask
                return $ dir!id

getUsers :: Query UserRepo [User]
getUsers = do UserRepo dir <- ask
              return . map snd . toList $ dir

-- the automagic

instance Version UserRepo
$(deriveSerialize ''UserRepo)

instance Component UserRepo where
  type Dependencies UserRepo = End
  initialValue = UserRepo empty  

$(mkMethods ''UserRepo ['addUser, 'getUser,'getUsers])

