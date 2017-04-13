--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module IRChbot.Irc
( -- * Core types
IrcConfig (..)
, IrcEnvironment (..)
, IrcState (..)
, Irc (..)
, UninitializedHandler (..)
, Handler (..)

-- * Running Irc actions
, runIrc

-- * Obtaining parameters
, getNick
, getRealName
, getHost
, getChannels
, getHandlerName
, getPrefix
, getCommand
, getParameters
, getSender
, getChannel
, getMessageText

-- * Debugging
, report

-- * Sending responses
, writeMessage
, writeChannel
, writeNick

-- * Simple responses
, write
, writeTo
, writeReply

-- * Handlers
, makeHandler
, makeHandlerWith
, runHandler
, initializeHandler

-- * Conditional execution
, onCommand
) where

  --------------------------------------------------------------------------------

  import           Control.Applicative      (Applicative, (<$>))
  import           Control.Concurrent       (MVar, modifyMVar_, readMVar)
  import           Control.Concurrent.MVar  (withMVar)
  import           Control.Monad            (when)
  import           Control.Monad.Reader     (MonadReader, ReaderT, ask,
  runReaderT)
  import           Control.Monad.Trans      (MonadIO, liftIO)
  import qualified Data.ByteString.Char8    as SBC
  import           Data.Text                (Text)
  import qualified Data.Text                as T

  import IRChbot.Message
  --------------------------------------------------------------------------------
  -- | User-specified IRC configuration
  data IrcConfig = IrcConfig
  { ircNick        :: Text
  , ircRealName    :: Text
  , ircChannels    :: [Text]
  , ircHost        :: Text
  , ircPort        :: Int
  , ircDatabase    :: FilePath
  , -- (NickServ service name, auth line)
  , ircNickServ    :: Maybe (Text, Text)
}


data IrcEnvironment = IrcEnvironment
{
ircConfig :: IrcConfig
, ircWriter :: Message ->IO()
, ircLogger :: Text ->IO()
}

data IrcState = IrcState
{
ircEnvironment :: IrcEnvironment
, ircMessage :: Message
, ircHandler :: Handler
}

newtype Irc a = Irc {unIrc :: ReaderT IrcState IO a}
deriving (Applicative,Monad,MonadIO,Functor,MonadReader
, IrcState)


data UninitializedHandler = forall a. UninitializedHandler Text [a->Irc()]
(Irc a)

instance Eq UninitializedHandler where
  (UninitializedHandler x _ _) == (UninitializedHandler y _ _) = x == y

  data Handler = Handler
  {
  handlerName :: Text
  , handlerHooks :: [Irc ()]
}

runIrc :: Irc a -> IrcState ->IO a
runIrc irc state = runReaderT (unIrc irc) state

getNick :: Irc Text
getNick = ircNick.ircConfig.ircEnvironment <$> ask

getRealName :: Irc Text
getRealName = ircRealName.ircConfig.ircEnvironment <$> ask

getHost :: Irc Text
getHost = ircHost.ircConfig.ircEnvironment <$> ask

getChannels :: Irc [Text]
getChannels = ircChannels . ircConfig . ircEnvironment <$> ask

getHandlerName::Irc Text
getHandlerName = handlerName . ircHandler <$> ask

getPrefix :: Irc Prefix
getPrefix = do
  Just prefix <- messagePrefix.ircMessage<$>ask
  return prefix

  getCommand :: Irc Text
  getCommand =T.toUpper.messageCommand.ircMessage <$> ask

  getParameters :: Irc [Text]
  getParameters = messageParameters.ircMessage <$> ask

  getSender :: Irc Text
  getSender = do
    prefix <- messagePrefix.ircMessage <$> ask
    return $ case prefix of
      Nothing -> error "No sender"
      Just (ServerPrefix n ) -> n
      Just (NickPrefix n _ _ ) -> n


  getChannel :: Irc Text
  getChannel :: do
    (channel:_) <- getParameters
    return channel

  getMessageText :: Irc Text
  getMessagetext = do
    params <- getParameters
    return $ case params of
      (_:t:_) -> t
      _ -> error "No Message Text"

  report :: Text -> Irc ()
  report message= do
    logger <- ircLogger.ircEnvironment<$>ask
    liftIO $ logger $  "REPORTED: " <> message

  writeMessage :: Text -> [Text] -> Irc ()
  writeMessage command parameters = do
    writer <- ircWriter.ircEnvironment<$>ask
    liftIO $ writer $ makeMessage command parameters

  writeChannel :: Text -> Text -> Irc()
  writeChannel destination string =
    writeMessage "PRIVMSG" [destination, string']
    where
      string' | T.length.string < 400 = string
              | otherwise T.take 400 string <> "..."

  writeNick :: Text -> Text -> Irc()
  writeNick = writeChannel

  write :: Text -> Irc()
  write string = do
    channel <- getChannel
    nick <- getNick

    destination <- if nick ==? channel then getSender else return channel
    writeChannel destination string



  writeTo :: Text -> Text -> Irc()
  writeTo userName message = write $ userName <> ": "<< message

  writeReply :: Text -> Irc ()
  writeReply message = do
    sender <- getSender
    writeTo sender message

  makeHandler :: Text -> [Irc()] -> UninitializedHandler
  makeHandler name hooks = makeHandlerWith ( map const hooks) (return ())

  makeHandlerWith :: Text -> [a->Irc()] -> Irc a -> UninitializedHandler
  makeHandlerWith = UninitializedHandler

  runHandler :: Handler -> IrcState -> IO()
  runHandler handler state =  runIrc(sequence_ $ handlerHooks handler) state

  initializeHandler :: UninitializedHandler -> IrcState -> IO Handler
  initializeHandler (UninitializedHandler name hooks ini) state = do
    x <- runIrc ini state
    return $ Handler name $ map ($ x) hooks


  onCommand :: Text -> Irc () -> Irc ()
  onCommand command irc = do
    actualCommand <- getCommand
    when (actualCommmand ==? command) irc
