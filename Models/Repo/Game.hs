{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, 
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             TemplateHaskell, TypeFamilies #-}
module Models.Repo.Game where
  
import Models.Repo.Types


-- data type

newtype GameRepo = GameRepo (Map Int Game)
                   deriving (Typeable, Data)

-- the operations

addGame :: User -> User -> Update GameRepo Game
addGame u1 u2 = do GameRepo r <- get
                   let id = (size r) + 1
                       g = create' u1 u2 id
                   put . GameRepo . insert id g $ r
                   return g

saveGame :: Game -> Update GameRepo ()
saveGame g = do GameRepo r <- get
                let r' = update (\_ -> Just g) (gID g) r
                put . GameRepo $ r'
                return ()

getGame :: Int -> Query GameRepo Game
getGame id = do GameRepo dir <- ask
                return $ dir!id

-- the automagic

instance Version GameRepo
$(deriveSerialize ''GameRepo)

instance Component GameRepo where
  type Dependencies GameRepo = End
  initialValue = GameRepo empty  

$(mkMethods ''GameRepo ['addGame, 'getGame,'saveGame])

