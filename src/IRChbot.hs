module IRChbot(
  IRChbot,
  IRChbot'
) where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (forM,forM_)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.Environment (getProgName)

--------------------------------------------------------------------------------
import           IRChbot.Application
import           IRChbot.Handlers
import           IRChbot.Irc
import           IRChbot.Logger
import           IRChbot.Message
import           IRChbot.SandBox

--------------------------------------------------------------------------------

application :: Logger -> [UnInitialisedHandler] -> IrcConfig -> Application
application logger uninitialised config writer = do
  let environment = config writer logger

  handlers' <-  fmap catMaybes $ forM uninitialised $
      \h@(UnInitialisedHandler name _ _) -> do
        let state = IrcState environment
                    (error "IRChbot: message not known yet")
                    (error "Uninitialised Handler")
        r <- sandbox logger name (Just 10) $ InitializeHandler h state
        case r of
          Nothing  -> logger $ "Could not initialize handler " <> name
          Just _ -> logger $ "Initialised handler " <> name

        return r

  return $ \msg ->
    forM handlers' $ \h -> do
      let state = IrcState environment msg h
      _ <- forkIO $ sandBox_ logger (handlerName h) (Just 60) $
          runHandler h state
          return()

--------------------------------------------------------------------------------

IRChbot :: IrcConfig -> IO()
IRChbot = IRChbot' handlers


IRChbot' :: [UnInitialisedHandler] -> IrcConfig -> IO()
IRChbot' handlers' config = do
  logname <- (++ ".log") <$> getProgName
  logger <- makeLogger logname
  runApplication logger (T.unpack $ IrcHost config) (IrcPort config ) $ application logger handlers' config
