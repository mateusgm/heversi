
-- | Utilities for generating random numbers.
module Random
where

import Config
import System.Random

-- | Choose on of the elements of this list at random.
chooseOneAtRandom :: [a] -> IO (Maybe a)
chooseOneAtRandom []	= return Nothing
chooseOneAtRandom xx
	| configRandomise
	= do	let len	= length xx
		n	<- getStdRandom (randomR (0, len - 1))
		return (Just (xx !! n))
	
	| otherwise
	=	return (Just (head xx))
