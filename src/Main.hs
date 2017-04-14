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
import           System.Environment      (getProgName)

import Write
import IMonad
import LogText
ircserver = "irc.freenode.org"
port   = 6667
channel   = "#tutbot-testing"
nick   = "BhadwiAunty"

-- type Net = ReaderT Bot IO
-- data Bot = Bot { socket :: Handle }

-- logger <- addLogText "LogFile.log"

main = bracket connect disconnect loop
    where
      disconnect = hClose.socket
      loop st = runReaderT run st
-- main = do
--     h <- connectTo ircserver (PortNumber (fromIntegral port))
--     hSetBuffering h NoBuffering
--     write h "NICK" nick
--     write h "USER" (nick++" 0 * :tutorial bot")
--     write h "JOIN" channel
--     listen h

connect :: IO Bot
connect = notify $ do
  h <- connectTo ircserver (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return (Bot h)
  where
    notify a = bracket_
                (printf "Connecting to %s....." ircserver >> hFlush stdout)
                (putStrLn "Done.")
                a
-- slice' :: Int -> Int -> List -> List
slice' start end xs = take (end -start) $ drop start xs
run::Net ()
run = do
      write  "NICK" nick
      write  "USER" (nick++" 0 * :tutorial bot")
      write  "JOIN" channel
      asks socket >>= listen

-- write :: String -> String -> Net ()
-- write  s t = do
--     h <- asks socket
--     io $ hPrintf h "%s %s\r\n" s t
--     io $ printf    "> %s %s\n" s t


listen :: Handle -> Net ()
listen h = do
  forever $ do
    s <- init `fmap` io(hGetLine h)
    -- if ping s then pong s else evl (clean s) (sender s) (receiver s)
    privateOrPublic s
    io(putStrLn s)
    -- putStrLn $ receiver t
  where
    forever a = do a; forever a
    clean = drop 1.dropWhile (/= ':').drop 1
    sender = drop 1.takeWhile(/= '!')
    receiver = takeWhile(/= '\SP').dropWhile(/= '#')

    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
--
-- evl :: String ->String ->String ->Net ()
-- evl "!quit" y z= write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
-- evl x y z
--   | "!id " `isPrefixOf` x  = if take 1 z == ['#'] then msg (drop 4 x)  else msgPrivate (drop 4 x) y
-- evl _ _ _ = return()

-- msg :: String->Net ()
-- msg str = write "PRIVMSG" (channel ++ " :" ++ str)
--
-- msgPrivate :: String->String->Net ()
-- msgPrivate str sender = write "PRIVMSG" (sender ++ " :" ++ str)

--a function to convert the IO monad back into the net Monad type
-- io:: IO a -> Net a
-- io = liftIO
