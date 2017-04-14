--------------------------------------------------------------------------------
------ | Implementing the logging process

module LogText
  (
  LogText,
  addLogText
  ) where


import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Data.Text
import qualified Data.Text.IO as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format(defaultTimeLocale, formatTime)
import qualified System.IO as IO

import IMonad
type LogText = Text -> IO ()

addLogText :: FilePath -> IO (Text -> IO ())
addLogText logname = do
  h <- IO.openFile logname IO.AppendMode
  -- lock <- newMVar ()
  return $ \bs -> bs `seq` do
    -- () <- takeMVar lock
    time <- formatTime defaultTimeLocale "%c" <$> getCurrentTime
    IO.hPutStr h time
    T.hPutStrLn h bs
    IO.hFlush h
    -- putMVar lock ()
