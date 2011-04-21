
-- | Hard coded game configuration.
module Config
where

-- | Clear the screen before each turn.
configClearScreen	= True

-- | Use VT100 color codes when drawing the board.
--	Try disabling this if you see junk like "[0;m" on the screen.
configColor		= True

-- | If there are multiple moves of equivalent goodness, 
--	then choose one at random instead of just taking the first.
--	This provides a more realistic game.
configRandomise		= False


-- | If backspace doesn't work on your system then try setting
--	this option to True. 
configAlternateGetLine	= False


 