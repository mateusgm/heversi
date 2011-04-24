module Controllers.Game (index, send, home) where

import Engine.Types     (Controller)
import Data.Map

index :: Controller
index _ = "Hello index!"

send  :: Controller
send a = "Hello send! " ++ show a

home  :: Controller
home _ = "Hello home!"
