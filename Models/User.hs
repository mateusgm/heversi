{-# LANGUAGE DeriveDataTypeable #-}
module Models.User where

  
import System.Templates
import Data.Data           (Data, Typeable)


data User = Android |
            Human String Int 
            deriving (Eq, Typeable, Data, Show)


human n i = Human n i
android = Android

isHuman Android = False
isHuman (Human _ _) = True
isAndroid = not . isHuman

uID :: User -> Int
uID Android     = 0
uID (Human _ i) = i

create :: String -> Int -> User
create name id = human name id


instance Infoable User where
   toMap (Human name id) = insert "name" name
                         . singleton "id" $ show id
          
