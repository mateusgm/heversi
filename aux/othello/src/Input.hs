{-# OPTIONS -fglasgow-exts #-}

module Input
where

import System.IO

-- | Get a line from the player.
--	We get the charaters one at a time so we can handle the backspace key
--	correctly.
getPlayerLine :: IO (Maybe String)
getPlayerLine 
	= getPlayerLine2 []


getPlayerLine2 :: String -> IO (Maybe String)
getPlayerLine2 prev
 = do	eof	<- hIsEOF stdin
	if eof 
	 then return Nothing 
	 else do
	 	c	<- hGetChar stdin
		getPlayerLineC prev c


getPlayerLineC :: String -> Char -> IO (Maybe String)
getPlayerLineC prev c	
	| '\n'	<- c	
		= return $ Just prev

	| elem c ['\DEL', '\b']	
	, prev == []
	= do	hPutStr stdout "\DEL "
		hFlush  stdout
		getPlayerLine2 prev

	| elem c ['\DEL', '\b']	
	, prev /= []
	= do	
		hPutStr stdout "\DEL \b"
		hFlush  stdout
		getPlayerLine2 (init prev) 

	| otherwise
	= do	getPlayerLine2 (prev ++ [c])	
