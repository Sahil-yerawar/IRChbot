module IRChbot.Message
  (
    Prefix (..)
  , Message (..)
  , makeMessage
  , (<>)
  , (==?)
  , toLower
  ) where

--------------------------------------------------------------------------------
  import qualified Data.Char   as Char (toLower)
  import           Data.Monoid (Monoid, mappend)
  import           Data.Text   (Text)
  import qualified Data.Text   as T

--------------------------------------------------------------------------------

data Prefix = ServerPrefix Text | NickPrefix Text (Maybe Text) (Maybe Text)
    deriving (Show,Eq)

data Message = Message
  {
    messagePrefix :: Maybe Prefix
  , messageCommand :: Text
  , messageParameters :: [Text]
  }deriving (Show, Eq)

makeMessage Text -> [Text] -> Message
makeMessage = Message Nothing

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

(==?) :: Text -> Text -> Bool
s1 ==? s2 = toLower s1 == toLower s2

toLower :: Text -> Text
toLower = T.map toLower'
where
  toLower' '[' = '{'
  toLower' ']' = '}'
  toLower' '\\' = '|'
  toLower' '~' = '^'
  toLower' x = Char.toLower x
