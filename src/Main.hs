--------------------------------------------------------------------------------
-- | Main module which is run first

module Main where

import Network
import System.IO
import Text.Printf
import Data.List
import qualified Data.Text as T
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Control.Applicative ((<$>))
import System.Environment (getProgName)

-- | user-defined modules
import Write
import IMonad
import LogText

-- | necessary main function which runs the application
main = bracket connect disconnect loop
    where
      disconnect = hClose.socket
      loop st = runReaderT run st

-- | function to connect to IRC server
connect :: IO Bot
connect = notify $ do
  h <- connectTo getIRCserver (PortNumber (fromIntegral getPort))
  hSetBuffering h NoBuffering
  return (Bot h)
  where
    notify a = bracket_
                (printf "Connecting to %s....." getIRCserver >> hFlush stdout)
                (putStrLn "Done.")
                a
-- | function to slice the element of the list
slice' start end xs = take (end -start) $ drop start xs

-- | function which registers the bot with its credentials once connected
run::Net ()
run = do
      write  "NICK" getNick
      write  "USER" (getNick++" 0 * :tutorial bot")
      write  "JOIN" getChannel
      asks socket >>= listen

-- | function which listens forever for the incoming messages and also prints
-- | outgoing messages to the server
listen :: Handle -> Net ()
listen h = do
  forever $ do
    s <- init `fmap` io(hGetLine h)
    -- if ping s then pong s else evl (clean s) (sender s) (receiver s)
    privateOrPublic s
    io(putStrLn s)

  where
    forever a = do a; forever a
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
