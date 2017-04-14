{-Some Function Using Random Number Generator -}

import System.Random
import System.IO.Unsafe  


rollDice = c::Int where
 	c = unsafePerformIO (getStdRandom (randomR (1, 6))) 



randomInt seed = (c::Int) `mod` 2
	where 
		c = unsafePerformIO (getStdRandom (randomR (0, 1)))



main = do
	print rollDice
	print (randomInt 0)