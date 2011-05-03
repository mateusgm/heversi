module Controllers.Game   (index, begin, play, get, end)
  where

import System.Types       (Controller)
import Data.Map


index :: Controller
index _ = "Hello index!"

begin :: Controller
begin m = "Hello begin!" 

play :: Controller
play m = "Hello play!" 

get :: Controller
get m   = "Hello get!"

end :: Controller
end m   = "Hello get!"
