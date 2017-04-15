--------------------------------------------------------------------------------
-- | module which defines all the constant data of the application
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module IMonad
(
  Bot (..)
, Net
, io
, getChannel
, getNick
, getIRCserver
, getPort
) where

import Control.Monad.Reader
import Control.Monad.Trans
import System.IO

-- | defining the net monad and the bot data structure
data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

-- | defining all the bot credentials for the IRC protocol
ircserver = "irc.freenode.org"
port   = 6667
channel   = "#POPLChannel"
nick   = "TheGeniusBot"

--a function to convert the IO monad back into the net Monad type
io:: IO a -> Net a
io = liftIO

-- | get functions to return several irc credential parameters

getChannel :: String
getChannel = channel

getIRCserver :: String
getIRCserver = ircserver

getNick :: String
getNick = nick

getPort :: Int
getPort = port
