{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}
module Models.Repo.User where
  
import Models.Repo.Types


-- data type

newtype UserRepo = UserRepo (Map Int User)
                   deriving (Typeable, Data)

-- the operations

addUser :: String -> Update UserRepo User
addUser n = do UserRepo r <- get
               let id = (size r) + 1
                   u = create n id
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

