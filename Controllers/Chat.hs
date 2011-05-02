module Controllers.Chat (index, send, get)
  where

import Engine.Types     (Controller)
import Data.Map



index :: Controller
index _ = "Hello index!"

send :: Controller
send m = "Hello send!" 

get :: Controller
get m   = "Hello get!"



