-- | Provides functions to deal with User commands
{-# LANGUAGE OverloadedStrings #-}
module IRChbot.User
    ( -- * Obtaining parameters
      getUserCommand
    , getUserCommandText

      -- * Handlers
    , makeUserHandler

      -- * Reacting on events
    , onCommand
    , onUserCommand
    , onUserCommands
    ) where

--------------------------------------------------------------------------------
import           Control.Monad  (forM_, when)
import           Data.Char      (isSpace)
import           Data.Text      (Text)
import qualified Data.Text      as T


--------------------------------------------------------------------------------
import           IRChbot.Irc
import           IRChbot.Util


--------------------------------------------------------------------------------

getUserCommand :: Irc Text
getUserCommand = flip fmap getMessageText $ T.takeWhile(not.isSpace)

getUserCommandText :: Irc Text
getUserCommandText = flip fmap getMessagetext $ T.strip. T.dropWhile(not.isSpace)

makeUserHandler :: Text -> [Text] -> (Text -> Irc Text) -> UninitializedHandler
makeUserHandler name commands f = makeHandler name $ return $
  onUserCommand commands $ getUserCommandText >>= f >>= write

onUserCommand::Text->Irc()->Irc()
onUserCommand command = onUserCommands [command]

onUserCommands::[Text]->Irc()->Irc()
onUserCommands commands irc = onCommand "PRIVMSG" $ do
  actualCommand <- getUserCommand
  forM_ commands $ \c -> when (actualCommand ==? c) irc
