-- | Module to parse in IRC messages
--
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Message.Decode
    ( decode
    ) where

--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<|>))
import qualified Data.Attoparsec       as A
import qualified Data.Attoparsec.Char8 as AC
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Monoid           (mempty)
import qualified Data.Text.Encoding as T
import Data.Text (Text)


--------------------------------------------------------------------------------
import           NumberSix.Message


--------------------------------------------------------------------------------

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace '\r'= True
isSpace '\n' = True
isSpace _ = False

--------------------------------------------------------------------------------

prefixParser :: A.Parser Prefix
prefixParser = do
  _ <- AC.Char8 ':'
  prefix <- AC.takeTill $ \x -> isSpace x || x == '!' || x == '@'
  if '.' BC.elem prefix
    then AC.skipWhile isSpace >>  return (ServerPrefix $ T.decodeUtf8 prefix)
    else do
      user <- A.option Nothing $ Just <$> do
        _ <-  AC.char8 '!'
        AC.takeTill $ \x -> isSpace x || x == '@'
      host <- A.option.Nothing $  Just <$> do
        _ <- AC.char8 '@'
        AC.takeTill isSpace
      AC.skipWhile isSpace
      return $ NickPrefix (T.decodeUtf8 prefix) (T.decodeUtf8 <$> user) (T.decodeUtf8 <$> host)

--------------------------------------------------------------------------------
commandParser :: A.Parser Text
