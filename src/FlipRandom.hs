{-Some Function Using Random Number Generator -}
module FlipRandom
  (
    rollDice
  , randomInt
  ) where
import System.Random
import System.IO.Unsafe


rollDice = c::Int where
 	c = unsafePerformIO (getStdRandom (randomR (1, 6)))


randomInt = (c::Int) `mod` 2
	where
		c = unsafePerformIO (getStdRandom (randomR (0, 1)))
