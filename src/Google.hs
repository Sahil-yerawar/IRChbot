--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Google
    ( google,
    sendertoio
    ) where
--------------------------------------------------------------------------------
import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (FromJSON (..),json, Value (..), (.:))
import           Data.Text            (Text)
import           Data.Aeson.Types     (parseEither)
import           Data.Attoparsec      (parseOnly)
import           Data.ByteString      (ByteString)
import qualified           Data.Text as T
import qualified           Data.Text.IO as T
--------------------------------------------------------------------------------
import           Http
--------------------------------------------------------------------------------
googleApiKey :: Text
googleApiKey = "AIzaSyBIYwie0BY-Txs92F5196V7iZb5Xn3cMxw"
--------------------------------------------------------------------------------
googleCseId :: Text
googleCseId = "015170067376393226585:deqxftulnbm"
--------------------------------------------------------------------------------
-- Result = list of item where each item is text text -like person {firstname Pankaj lastname Kukreja}
data Result = Result [Item] deriving (Show)
-- In above example "firstname pankaj" is item (lastname kukreja is another item)
data Item = Item Text Text deriving (Show)
--------------------------------------------------------------------------------
-- From Paresed Result to get items { where results are there } [see sampleresponse.txt (line 32)]
-- If no items then error occured
instance FromJSON Result where
    parseJSON (Object o) = Result <$> o .: "items"
    parseJSON _          = mzero
--------------------------------------------------------------------------------
-- get first title and link and return it as Item datatype
instance FromJSON Item where
    parseJSON (Object o) = Item <$> o .: "title" <*> o .: "link"
    parseJSON _          = mzero
--------------------------------------------------------------------------------
-- Returns the Title and URL (as IO text) of the first found link
google :: Text -> Text -> IO Text
google query sender= do
    json <- http url id -- get complete response of searched
    case parseJsonEither json of -- Parse response
        Right (Result (Item title link : _)) -> textAndUrl title link sender -- textAndUrl = Convert text to a IO TEXT
        -- title link = Right (First) of result and extract title and link
  where
    url = "https://www.googleapis.com/customsearch/v1" <>
        "?q=" <> urlEncode query <>
        "&key=" <> googleApiKey <>
        "&cx=" <> googleCseId <>
        "&alt=json"
--------------------------------------------------------------------------------
-- Parse JSON from a bytestring complete response is a text file and we parse it by json
-- Either used coz parsed can be error if invalid bytestring
-- parseonly - parse one at a time { there are multiple {} under items ... parseonly parses one at time}
-- Here return is data (Either a b) = Either String b
parseJsonEither :: FromJSON a => ByteString -> Either String a
parseJsonEither bs = parseOnly json bs >>= parseEither parseJSON

sendertoio:: String -> IO Text
sendertoio sender = return $ T.pack("PRIVMSG " ++ sender ++ " :")
--------------------------------------------------------------------------------
-- Use = google "text"
-- main = google "Eminem"
