--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts          #-}
module Write
  (
  write,
  msg,
  msgPrivate,
  privateOrPublic
  )
  where

import IMonad
import Google
import Http
import FlipRandom


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
import System.Exit

write :: String -> String -> Net ()
write  s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t



getSender :: String -> String
getSender  = takeWhile(/= '!').drop 1

getReceiver :: String->String
getReceiver  = takeWhile(/= '\SP').drop 1.dropWhile(/= '\SP').
      drop 1.dropWhile(/= '\SP')

privateOrPublic :: String ->  Net ()
privateOrPublic message
  | getReceiver message == getNick = evalPri message
  | otherwise = evalPub message

evalPri :: String -> Net()
evalPri message = do
  if ping message then pong message else evalPrivate (clean message)
                       (getSender message)  (getReceiver message)
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
    clean = drop 1.dropWhile (/= ':').drop 1

evalPub :: String -> Net()
evalPub message = do
  if ping message then pong message else evalPublic (clean message)
                       (getSender message)  (getReceiver message)
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
    clean = drop 1.dropWhile (/= ':').drop 1

evalPrivate ::String -> String -> String -> Net()
evalPrivate "!quit" y z= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalPrivate x y z
  | "!id " `isPrefixOf` x  = msgPrivate (drop 4 x) y
  | "!dec2bin " `isPrefixOf` x = dec2bin (drop 9 x) y
  | "!bin2dec " `isPrefixOf` x = bin2dec (drop 9 x) y
  | "!google " `isPrefixOf` x = gQuery (drop 8 x) y
  | "!gmt_time" `isPrefixOf` x = getTime y
  | "!local_time" `isPrefixOf` x = getLocalTime y
evalPrivate _ _ _ = return()

evalPublic ::String -> String -> String -> Net()
evalPublic "!quit" y z= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalPublic x y z
   | "!id " `isPrefixOf` x  =  msg (drop 4 x) y
   | x == "!randomCoin "  = rCoin y
   | x == "!rDie " `isPrefixOf` x = rDie  y

evalPublic _ _ _ = return()

msg :: String->String ->Net ()
msg str sender= do
  write "PRIVMSG" (getChannel ++ " :" ++ sender ++": "++ str)

msgPrivate :: String->String->Net ()
msgPrivate str sender = write "PRIVMSG" (sender ++ " :" ++ str)
--
-- msgPrivate' :: String->String->Net ()
-- msgPrivate' sender str = write' "PRIVMSG" (sender ++ " :" ++ str)
--
-- write' :: String -> String -> Net ()
-- write'  s t = do
--     h <- asks socket
--     io $ hPrint h
--     io $ print

getTime ::String -> Net()
getTime sender = do
  let a = unsafePerformIO(getCurrentTime)
  msgPrivate (show a) sender

getLocalTime ::String -> Net()
getLocalTime sender = do
  let a = unsafePerformIO(getZonedTime)
  msgPrivate (show a) sender

rCoin :: String -> Net()
rCoin sender = msg (show (randomInt 0)) sender

rDie :: String -> Net()
rDie  sender =   msg (show rollDice ) sender

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse(helpBin n)

helpBin 0 = []
helpBin n | n `mod` 2 == 1 = 1:helpBin(n `div` 2)
          | n `mod` 2 == 0 = 0:helpBin(n `div` 2)

dec2bin :: String -> String ->Net()
dec2bin num sender = do
  -- io $ printf "In dec2bin->\n"
  let number = read num :: Int
  msgPrivate (intToDigit `map` (toBin number)) sender

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0


bin2dec :: String -> String -> Net()
bin2dec num sender = do
  let number = toDec num
  msgPrivate (show number) sender

gQuery :: String -> String ->Net()
gQuery search sender = msgPrivate (T.unpack (google1 (T.pack search))) sender
