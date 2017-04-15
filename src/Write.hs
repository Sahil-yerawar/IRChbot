--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts          #-}
-- | write module implements most of the utilities of the bot
module Write
  (
  write,
  msg,
  msgPrivate,
  privateOrPublic
  )
  where
--------------------------------------------------------------------------------
-- | User defined modules
import IMonad
import Google
import Http
import FlipRandom
import CowsAndBulls
import WeatherForecast

--------------------------------------------------------------------------------
-- | Pre-Defined System Modules
import System.IO
import System.IO.Unsafe
import Text.Printf
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import Data.List
import Data.Char
import Data.Time
import Data.String
import Data.IORef
import System.Exit

--------------------------------------------------------------------------------
-- | simulation of global variables and functions to manipulate them
guessNum = unsafePerformIO $ newIORef 0
count = unsafePerformIO $ newIORef 19

newNode1 num num1= do
                      i <- readIORef num1
                      writeIORef num1 num
                      writeDummy "Sahil"

newNode3 num1= do
                      i <- readIORef num1
                      modifyIORef num1 sub
                      writeDummy "Sahil"
newNode2 num =  do
                      i <- readIORef num
                      writeIORef num i
                      return i
-- minus by 1 function
sub :: Int -> Int
sub h =  h-1

-- writes down dummy strings
writeDummy :: String -> IO()
writeDummy dummy = printf dummy

--------------------------------------------------------------------------------
-- | main write operation which inputs protocoled message in the socket
write :: String -> String -> Net ()
write  s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- | function to get the sender of the recent message
getSender :: String -> String
getSender  = takeWhile(/= '!').drop 1

-- | function to get the receiver of the message
getReceiver :: String->String
getReceiver  = takeWhile(/= '\SP').drop 1.dropWhile(/= '\SP').
      drop 1.dropWhile(/= '\SP')

-- | decider function to check whether the next message is private or public
privateOrPublic :: String ->  Net ()
privateOrPublic message
  | getReceiver message == getNick = evalPri message
  | otherwise = evalPub message

-- | separate evaluate for the private commands
evalPri :: String -> Net()
evalPri message = do
  if ping message then pong message else evalPrivate (clean message)
                       (getSender message)  (getReceiver message)
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
    clean = drop 1.dropWhile (/= ':').drop 1

-- | separate evaluate for the public commands
evalPub :: String -> Net()
evalPub message = do
  if ping message then pong message else evalPublic (clean message)
                       (getSender message)  (getReceiver message)
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
    clean = drop 1.dropWhile (/= ':').drop 1

-- | guarded function to decide which private command to execute
evalPrivate ::String -> String -> String -> Net()
evalPrivate "!quit" y z= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalPrivate x y z
  | "!id " `isPrefixOf` x  = msgPrivate (drop 4 x) y
  | "!dec2bin " `isPrefixOf` x = dec2bin (drop 9 x) y
  | "!bin2dec " `isPrefixOf` x = bin2dec (drop 9 x) y
  | "!google " `isPrefixOf` x = gQuery (drop 8 x) y
  | "!gmt_time" `isPrefixOf` x = getTime y
  | "!local_time" `isPrefixOf` x = getLocalTime y
  | x == "!cowsNbulls" = initCnB y
  | "!guess " `isPrefixOf` x = guessNumber (drop 7 x) y
evalPrivate _ _ _ = return()

-- | guarded function to decide which private command to execute
evalPublic ::String -> String -> String -> Net()
evalPublic "!quit" y z= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalPublic x y z
   | "!id " `isPrefixOf` x  =  msg (drop 4 x) y
   | x == "!randomCoin"  = rCoin y
   | x == "!rDie"  = rDie  y
   | x == "!weather" = weather y
evalPublic _ _ _ = return()

msg :: String->String ->Net ()
-- | Separate messaging command for public commands
msg str sender= do
  write "PRIVMSG" (getChannel ++ " :" ++ sender ++": "++ str)

-- | Separate messaging command for private commands
msgPrivate :: String->String->Net ()
msgPrivate str sender = write "PRIVMSG" (sender ++ " :" ++ str)


--function to get current time in GMT form
getTime ::String -> Net()
getTime sender = do
  let a = unsafePerformIO(getCurrentTime)
  msgPrivate (show a) sender

-- | function to get current time according to their respective time zone
getLocalTime ::String -> Net()
getLocalTime sender = do
  let a = unsafePerformIO(getZonedTime)
  msgPrivate (show a) sender

-- | function to simulate tossing of a coin
rCoin :: String -> Net()
rCoin sender | randomInt == 0 = msg "Heads" sender
             | otherwise = msg "Tails" sender

-- | function to simulate throwing of a die
rDie :: String -> Net()
rDie  sender =   msg ("The Number on The Die is "++(show rollDice)) sender

-- | function to initiate the cows and bulls game
initCnB ::String -> Net()
initCnB sender = do
  io $ printf "Inside Init of CnB"
  io $ newNode1 randomNumber guessNum
  io $ newNode1 10 count
  msgPrivate "Guess the number, Your Time Starts Now" sender

-- | function to evaluate user guess in the game
guessNumber :: String -> String -> Net()
guessNumber number sender = if unsafePerformIO(readIORef count) == 0 then msgPrivate "Sorry, Time's up!" sender else do
    io $ newNode3 count
    let b = getNumList (read number ::Int)
    let c = checkCows b (getNumList (unsafePerformIO(readIORef guessNum))) 3
    let d = (checkBulls b (getNumList (unsafePerformIO(readIORef guessNum)))) - c
    msgPrivate ("Turn" ++ (intToDigit(11- unsafePerformIO(newNode2 count)):[]) ++ " : "++"Cows = "++ (show c)++"Bulls = "++ (show d)) sender

-- | helper function to convert to binary
toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse(helpBin n)

-- | another helper function to help in converting to binary
helpBin 0 = []
helpBin n | n `mod` 2 == 1 = 1:helpBin(n `div` 2)
          | n `mod` 2 == 0 = 0:helpBin(n `div` 2)

-- | function to convert decimal to binary
dec2bin :: String -> String ->Net()
dec2bin num sender = do
  let number = read num :: Int
  msgPrivate (intToDigit `map` (toBin number)) sender

-- |helper function to convert to decimal
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- |function to convert decimal to binary
bin2dec :: String -> String -> Net()
bin2dec num sender = do
  let number = toDec num
  msgPrivate (show number) sender

-- |function to search for a google query
gQuery :: String -> String ->Net()
gQuery search sender = msgPrivate (T.unpack (google1 (T.pack search))) sender

-- |function to get current weather
weather :: String -> Net()
weather sender = msg (unsafePerformIO(getForeCast)) sender
