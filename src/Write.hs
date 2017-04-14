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

import System.IO
import Text.Printf
import Control.Monad.Reader
import Data.List
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
evalPrivate _ _ _ = return()

evalPublic ::String -> String -> String -> Net()
evalPublic "!quit" y z= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalPublic x y z 
   | "!id " `isPrefixOf` x  =  msg (drop 4 x) y

evalPublic _ _ _ = return()

msg :: String->String ->Net ()
msg str sender= do
  write "PRIVMSG" (getChannel ++ " :" ++ sender ++": "++ str)

msgPrivate :: String->String->Net ()
msgPrivate str sender = write "PRIVMSG" (sender ++ " :" ++ str)
