--------------------------------------------------------------------------------
-- | HTTP and HTML utility functions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Http
    ( http
    , (<>)
    , Doc (..)
    , httpPrefix
    , urlEncode
    , textAndUrl
    ) where
--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import           Text.XmlHtml
import           Text.XmlHtml.Cursor
import           Data.Monoid (Monoid, mappend)
import           Debug.Trace            (trace)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Network.HTTP.Conduit   as HC
import qualified Network.HTTP.Types.URI as URI

--------------------------------------------------------------------------------
data Doc = Html | Xml deriving (Show)
type RequestOptions = HC.Request -> HC.Request
--------------------------------------------------------------------------------
-- | Perform an HTTP get request and return the response body
http :: Text            -- ^ URL 
     -> RequestOptions  -- ^ Set extra request options
     -> IO ByteString   -- ^ Response body
http uri modifyReq = do
    req <- HC.parseUrl uri' --URL to request -> All information on how to connect to a host and what should be sent in the HTTP request.
    mgr <- HC.newManager HC.tlsManagerSettings --Manager Keeps track of open connections for keep-alive.
    rsp <- flip HC.httpLbs mgr $ modifyReq $ setUserAgent "IRCBOT/1.0.0" req -- Download the specified Request as a Response.
    HC.closeManager mgr --Close Connection
    return $ B.concat $ BL.toChunks $ HC.responseBody rsp -- return Response as IO bytestring 
  where
    uri'               = T.unpack $ httpPrefix uri -- uri' = http in start of url
    setUserAgent ua rq =
        rq {HC.requestHeaders = ("User-Agent", ua) : HC.requestHeaders rq}

--------------------------------------------------------------------------------
-- | Add "http://" to the given URL, if needed
httpPrefix :: Text -> Text
httpPrefix url
    | "http://" `T.isPrefixOf` url || "https://" `T.isPrefixOf` url  = url
    | otherwise = "http://" <> url


--------------------------------------------------------------------------------
-- | Encode a ByteString to an URL
-- Encode a value as x-www-urlencoded
urlEncode :: Text -> Text
urlEncode = T.decodeUtf8 . URI.urlEncode True . T.encodeUtf8

--------------------------------------------------------------------------------
-- Concat text and url and return as IO Text
textAndUrl :: Text -> Text -> IO Text
textAndUrl text url = return $ join text url
  where
    join t u = if T.null t then u else t <> " >> " <> u

--------------------------------------------------------------------------------
-- <> is custom symbol = mappend ... combine 2 text
(<>) :: Monoid m => m -> m -> m
(<>) = mappend