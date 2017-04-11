import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit


ircserver = "irc.freenode.org"
port   = 6667
channel   = "#tutbot-testing"
nick   = "BhadwiAunty"

main = do
    h <- connectTo ircserver (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :tutorial bot")
    write h "JOIN" channel
    listen h

-- slice' :: Int -> Int -> List -> List
slice' start end xs = take (end -start) $ drop start xs
write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    let t = init s
    if ping t then pong t else evl h (clean t) (sender t) (receiver t)
    putStrLn s
    -- putStrLn $ receiver t
  where
    forever a = do a; forever a
    clean = drop 1.dropWhile (/= ':').drop 1
    sender = drop 1.takeWhile(/= '!')
    receiver = takeWhile(/= '\SP').dropWhile(/= '#')

    ping x = "PING :" `isPrefixOf` x
    pong x = write h "PONG" (':' : drop 6 x)

evl :: Handle ->String ->String ->String ->IO ()
evl h "!quit" y z= write h "QUIT" ":Exiting" >> exitWith ExitSuccess
evl h x y z
  | "!id " `isPrefixOf` x  = if take 1 z == ['#'] then msg h (drop 4 x)  else msgPrivate h (drop 4 x) y
evl _ _ _ _ = return()

msg :: Handle->String->IO ()
msg buffer str = write buffer "PRIVMSG" (channel ++ " :" ++ str)

msgPrivate :: Handle->String->String->IO ()
msgPrivate buffer str sender = write buffer "PRIVMSG" (sender ++ " :" ++ str)
