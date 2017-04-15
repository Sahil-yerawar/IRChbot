{-#LANGUAGE ScopedTypeVariables#-}
{- Cows And Bulls -}
module CowsAndBulls
  (
  randomNumber,
  getNumList,
  checkCows,
  checkBulls
  ) where


import System.IO.Unsafe
import System.IO
import Data.List
import Data.Char
import Control.Monad (replicateM)
import System.Random (randomIO, randomRIO, randomR,getStdRandom)

checkBulls :: [Int] -> [Int] -> Int
checkBulls [] b = 0
checkBulls (x:xs) b = if x `elem` b then (1+(checkBulls (xs) b)) else (checkBulls (xs) b)


checkCows :: [Int] -> [Int] -> Int -> Int
checkCows a b 0 = if (a!!0 == b!!0) then 1 else 0
checkCows a b c = if ((a!!c) == (b!!c)) then (1 + (checkCows a b (c-1)))
                          else (checkCows a b (c-1))

getNumList ::Int ->  [Int]
getNumList n = [(div n 1000) `mod` 10 ,(div n 100) `mod` 10 ,(div n 10) `mod` 10 , n `mod` 10 ]

game :: Int->[Int]->IO ()
game 0 l=do
	print ("No more Turns left.\n"++"You are out of Luck Buddy!! Try again next time!!")
game n l= do
	print ("Turn" ++ (intToDigit(11-n):[]) ++ ": ")
	-- a <- getLine
	let b = getNumList (read "1234" :: Int)
	let c = checkCows b l 3
	let d = (checkBulls b l) - c
	print ("Cows = "++ (show c))
	print ("Bulls = "++ (show d))

	-- if c == 4 then print ("You Got it Buddy") else game (n-1) l
{-Some Function Using Random Number Generator -}
randomNumber :: Int
randomNumber = unsafePerformIO (getStdRandom (randomR (1023, 9876)))

game1 = do
	let c::Int = randomNumber
	let ansList= getNumList c
	game 10 ansList
