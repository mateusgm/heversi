module Controllers.Game (index, send, home) where

import Data.Map

index :: Map String String -> String
index _ = "Hello index!"

send :: Map String String -> String
send a = "Hello send! " ++ show a

home :: Map String String -> String
home _ = "Hello home!"
