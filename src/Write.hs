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
import Try

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
  | "!echo " `isPrefixOf` x  = msgPrivate (drop 6 x) y
  | "!dec2bin " `isPrefixOf` x = dec2bin (drop 9 x) y
  | "!bin2dec " `isPrefixOf` x = bin2dec (drop 9 x) y
  | "!google " `isPrefixOf` x = gQuery (drop 8 x) y
  | "!gmt_time" `isPrefixOf` x = getTime y
  | "!local_time" `isPrefixOf` x = getLocalTime y
  | x == "!cowsNbulls" = initCnB y
  | "!guess " `isPrefixOf` x = guessNumber (drop 7 x) y
  | x == "!commands" = dispHelp y
  | x == "!echo@help" = echHelp y
  | x == "!dec2bin@help" = d2bHelp y
  | x == "!bin2dec@help" = b2dHelp y
  | x == "!google@help" = gHelp y
  | x == "!gmt_time@help" = gtHelp y
  | x == "!local_time@help" = ltHelp y
  | x == "!cowsNBulls@help" = cnbHelp y
  | x == "!guess@help" = guHelp y
  | x == "!randomCoin@help" = rcHelp y
  | x == "!rDie@help" = rdHelp y
  | x == "!weather" = wHelp y
evalPrivate _ _ _ = return()

-- | guarded function to decide which private command to execute
evalPublic ::String -> String -> String -> Net()
evalPublic "!quit" y z= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalPublic x y z
   | "!echo " `isPrefixOf` x  =  msg (drop 6 x) y
   | x == "!randomCoin"  = rCoin y
   | x == "!local_time"  = getLocalTime' y
   | x == "!rDie"  = rDie  y
   | x == "!weather" = weather y
   | x == "!commands" = dispHelp' y
   | x == "!echo@help" = echHelp' y
   | x == "!dec2bin@help" = d2bHelp' y
   | x == "!bin2dec@help" = b2dHelp' y
   | x == "!google@help" = gHelp' y
   | x == "!gmt_time@help" = gtHelp' y
   | x == "!local_time@help" = ltHelp' y
   | x == "!cowsNBulls@help" = cnbHelp' y
   | x == "!guess@help" = guHelp' y
   | x == "!randomCoin@help" = rcHelp' y
   | x == "!rDie@help" = rdHelp' y
   | x == "!weather" = wHelp' y
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

getLocalTime' ::String -> Net()
getLocalTime' sender = do
  let a = unsafePerformIO(getZonedTime)
  msg (show a) sender

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
    msgPrivate ("Turn " ++ (intToDigit(11- unsafePerformIO(newNode2 count)):[]) ++ " : "++"Cows = "++ (show c)++" Bulls = "++ (show d)) sender

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


wHelp :: String -> Net()
wHelp sender = msgPrivate weatherHelp sender

echHelp :: String -> Net()
echHelp sender = msgPrivate echoHelp sender

dispHelp :: String -> Net()
dispHelp sender = msgPrivate displayHelp sender

d2bHelp :: String -> Net()
d2bHelp sender = msgPrivate dec2binHelp sender

b2dHelp :: String -> Net()
b2dHelp sender = msgPrivate bin2decHelp sender

gHelp :: String -> Net()
gHelp sender = msgPrivate googleHelp sender

gtHelp :: String -> Net()
gtHelp sender = msgPrivate gmt_timeHelp sender

ltHelp :: String -> Net()
ltHelp sender = msgPrivate local_timeHelp sender

cnbHelp :: String -> Net()
cnbHelp sender = msgPrivate cowsNbullsHelp sender

guHelp :: String -> Net()
guHelp sender = msgPrivate guesssHelp sender

rcHelp :: String -> Net()
rcHelp sender = msgPrivate randomCoinHelp sender

rdHelp :: String -> Net()
rdHelp sender = msgPrivate rDieHelp sender

dispHelp' :: String -> Net()
dispHelp' sender = msg displayHelp sender

d2bHelp' :: String -> Net()
d2bHelp' sender = msg dec2binHelp sender

b2dHelp' :: String -> Net()
b2dHelp' sender = msg bin2decHelp sender

gHelp' :: String -> Net()
gHelp' sender = msg googleHelp sender

gtHelp' :: String -> Net()
gtHelp' sender = msg gmt_timeHelp sender

ltHelp' :: String -> Net()
ltHelp' sender = msg local_timeHelp sender

cnbHelp' :: String -> Net()
cnbHelp' sender = msg cowsNbullsHelp sender

guHelp' :: String -> Net()
guHelp' sender = msg guesssHelp sender

rcHelp' :: String -> Net()
rcHelp' sender = msg randomCoinHelp sender

rdHelp' :: String -> Net()
rdHelp' sender = msg rDieHelp sender


echHelp' :: String -> Net()
echHelp' sender = msg echoHelp sender

wHelp' :: String -> Net()
wHelp' sender = msg weatherHelp sender

-- dispHelp :: String -> Net()
-- dispHelp sender = msgPrivate displayHelp sender
