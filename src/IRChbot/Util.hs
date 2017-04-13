module IRChbot.Util
    ( sleep
    , forkIrc
    , (<>)
    , (==?)
    , toLower
    , breakWord
    , prettyList
    , removeNewlines
    , randomElement
    , parseJsonEither
    , readText
    , maxLineLength
    ) where

--------------------------------------------------------------------------------
import           Control.Arrow        (second)
import           Control.Concurrent   (forkIO, threadDelay)
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON, json, parseJSON)
import           Data.Aeson.Types     (parseEither)
import           Data.Attoparsec      (parseOnly)
import           Data.ByteString      (ByteString)
import           Data.Char            (isSpace)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           System.Random        (randomRIO)


--------------------------------------------------------------------------------
import           IRChbot.Irc
import           IRChbot.Message


--------------------------------------------------------------------------------

sleep :: Double -> Irc ()
sleep x = liftIO $ threadDelay (round $ x * 1000000)

forkIrc :: Irc() -> Irc()
forkIrc irc = do
  _ <- liftIO.forkIO.runIrc irc =<< ask
  return ()
breakWord :: Text -> (Text, Text)
breakword = second(T.drop 1) . T . break isSpace

prettyList :: [Text] -> Text
prettyList [] = "None"
prettyList (x:[]) = x
prettyList (x:y:[]) = x <> " and " <> y
prettyList (x:y:z:r) = x <> ", " <> prettyList (y:z:r)\

removeNewlines :: Text -> Text
removeNewlines = T.map (\x -> if x `elem` ['\r','\n'] then ' ' else x)

randomElement:: [a] -> IO a
randomElement ls = fmap (ls !!) $ randomRIO(0,length ls-1)

parseJsonEither :: FromJSON a => ByteString -> Either String a
parseJsonEither bs = parseOnly json bs >>= parseEither parseJSON

readText :: Read a => Text ->Maybe a
readText t = case reads (T.unpack t) of
  [(x, "")] -> Just x
  _ -> Nothing

maxLineLength = 450      
