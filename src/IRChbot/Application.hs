--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module IRChbot.Application
  ( Application
  , runApplication
  ) where

--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Concurrent.MVar   (newMVar, putMVar, takeMVar)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.Monoid               (mappend, mempty)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Network.Socket            (Socket)
import qualified Network.Socket            as S
import           Network.Socket.ByteString


--------------------------------------------------------------------------------
import           IRChbot.Logger
import           IRChbot.Message
import           IRChbot.Message.Decode
import           IRChbot.Message.Encode
import           IRChbot.Util


--------------------------------------------------------------------------------
type Application = (Message -> IO ()) -> IO (Message -> IO ())

--------------------------------------------------------------------------------
--- | Run a single IRC application

runApplication :: Logger -> String -> Int -> Application -> IO()
runApplication logger host port application = do
  sock <- connectToSocket
  writer <- makeMessageWriter logger sock
  app <- application writer
  go sock app mempty
  logger "Server closed Safe and clean"
  S.sClose sock
  where
    connectToSocket = do
      addr <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
      sock <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
      S.setSocketOption sock S.KeepAlive 1
      S.connect sock $ S.addrAddress  addr
      return sock

    go sock app state = do
      nmsg <- readMessage logger sock state
      case nmsg of
        Nothing      -> return ()
        Just (msg, state') -> app msg >> go sock app state'



  type ReadState = ByteString

  readLine :: Socket -> ReadState -> IO(ByteString -> IO())
  readLine sock chunk
    | B.null.rest = receiveMore
    | otherwise = return $ Just (line, B.drop 2 rest)
  where
    (line, rest) = BC.breakSubstring "\r\n" chunk
    receiveMore = do
      more <- recv sock 4096
      if B.null more
        then return Nothing
        else readline sock $ mappend chunk more

--------------------------------------------------------------------------------
readMessge :: Logger -> Socket ->ReadState -> IO(Maybe(Message, ReadState))
readMessge logger sock state = do
  mLine <- readLine sock state
  case mLine of
    Nothing -> return Nothing
    Just (line, state') -> case decode line of
      Just msg -> do
        logger $ T.pack $"IN: " ++ show msg
        return $ Just (msg, state')
      Nothing -> do
        logger $ "IRChbot.Socket.readMessage: Can't Parse: " T.append T.decodeUtf8line
        readMessage logger sock state'

--------------------------------------------------------------------------------
makeLineWriter :: Socket -> IO(ByteString ->IO())
makeLineWriter sock = do
  lock <- newMVar ()
  return $ \bs -> bs `seq` do
    () <- takeMVar lock
    sendAll sock $ bs mappend "\r\n"
    putMVar lock ()

--------------------------------------------------------------------------------
makeMessageWriter :: Logger -> Socket -> IO(Message -> IO())
makeMessageWriter logger sock = do
  lineWriter <- makeLineWriter sock
  return $ \msg -> do
    let bs = encode msg
        san = B.take maxLineLength $ B.takeWhile ( B.notElem "\r\n") bs
    logger $ T.pack $ "OUT: " ++ show msg
    lineWriter san 
