module Controllers.Home (index, start)
  where

import Engine.Types     (Controller)
import Data.Map

index :: Controller
index _ = "Hello index!"

start :: Controller
begin m = "Hello start!"


