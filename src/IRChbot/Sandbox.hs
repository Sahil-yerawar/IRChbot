--------------------------------------------------------------------------------
-- | Provides a sandbox in which plugins can run
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NumberSix.SandBox
    ( sandBox
    , sandBox_
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent      (forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import           Control.Exception       (SomeException, try)
import           Data.Monoid             (mappend)
import           Data.Text               (Text)
import qualified Data.Text               as T


--------------------------------------------------------------------------------
import           NumberSix.Logger


--------------------------------------------------------------------------------

data Signal a = Finished a | Crashed Text | Timeout deriving(Show, Eq)

sandBox :: ∀ a.Logger -> Text -> Maybe Int -> IO a -> IO (Maybe a)
sandBox logger name timeout action = do
  mvar <- newEmptyMVar
  actionThreadId <- forkIO $ do
    r <- try action
    putMVar mvar $ case (r :: Either SomeException a ) of
      Left e -> Crashed (T.pack $ show e)
      Right x -> Finished x

    timeoutThreadId <- case timeout of
      Nothing -> return Nothing
      Just t -> fmap Just $ forkIO $ do
        threadDelay (t * 1000000)
        putMVar mvar Timeout

    result <- readMVar mvar
    case result of
      Timeout -> killThread actionThreadId
      _       -> case timeoutThreadId of
        Just id' -> killThread id'
        Nothing -> return ()

    case result of
      Crashed e -> logger $
                    "Thread" `mappend` name `mappend` " crashed: " `mappend` e
      Timeout -> logger $
                  "Thread" `mappend` name `mappend` "timed out"

    case result of
      Finished x -> return (Just x)
      _          -> return Nothing


sandBox_ :: Logger -> Text -> Maybe Int -> IO a -> IO ()
sandBox_ logger name timeout f = do
  _ <- sandbox logger name timeout f
  return ()
