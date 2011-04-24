module Controllers.Game (game) where

game :: [String] -> [(String, String)] -> String
game [] _ = "Hello World"
game s (x:xs) = show s ++ "     " ++ a ++ " " ++ b
  where (a,b) = x
game s _ = show s  
