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

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

ircserver = "irc.freenode.org"
port   = 6667
channel   = "#tutbot-testing"
nick   = "BhadwiAunty"

--a function to convert the IO monad back into the net Monad type
io:: IO a -> Net a
io = liftIO

getChannel :: String
getChannel = channel

getIRCserver :: String
getIRCserver = ircserver

getNick :: String
getNick = nick

getPort :: Int
getPort = port
