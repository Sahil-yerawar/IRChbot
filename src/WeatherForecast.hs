{-# LANGUAGE OverloadedStrings #-}
{- Weather Information Print using API of Forecast.io -}

module WeatherForecast
  (
  getForeCast
  ) where

import System.Environment
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.List
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.IO.Unsafe
------------------------------------------------------------------------------------------------------------
-- |This is the main function
getForeCast = do
    let endpoint = "https://api.forecast.io/forecast/"
    let apiKey   = "67370187757e6dc2f944e6b8ed41d912"
    let lat      = "17.3850"
    let lon      = "78.4867"
    simpleHttp (endpoint++apiKey++"/"++lat++","++lon) >>= (L.writeFile "forecast.txt")
    forecastData <- L.readFile "forecast.txt"
    let forecast = decode forecastData :: Maybe Forecast
    return (getWeather forecast)

-----------------------------------------------------------------------------------------------------------
getWeather :: Maybe Forecast -> String
getWeather (Just (Forecast {currently = current})) = ("Current weather: " ++ (show (summary current)) ++
                                                        ", " ++ (show (temperature current)) ++ "F")
-----------------------------------------------------------------------------------------------------------
getMyWeather = s::String where
    s = unsafePerformIO(getForeCast)
-----------------------------------------------------------------------------------------------------------
-- |The currently object
data Currently = Currently
    { summary     :: T.Text
    , temperature :: Float
    } deriving Show
-----------------------------------------------------------------------------------------------------------
-- |The forecast object
data Forecast = Forecast
    { latitude  :: Float
    , longitude :: Float
    , timezone  :: T.Text
    , currently :: Currently
    } deriving Show

-----------------------------------------------------------------------------------------------------------
--  Code to create Currently object from JSON
--  Helper to Decode Forecast object
instance FromJSON Currently where
    parseJSON (Object v) = Currently
                           <$> v .: (T.pack "summary")
                           <*> v .: (T.pack "temperature")
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero

-----------------------------------------------------------------------------------------------------------
-- Code to create forecast object from JSON
-- used by decode funtion
instance FromJSON Forecast where
    parseJSON (Object v) = Forecast
                           <$> v .: (T.pack "latitude")
                           <*> v .: (T.pack "longitude")
                           <*> v .: (T.pack "timezone")
						   <*> v .: (T.pack "currently")
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
-----------------------------------------------------------------------------------------------------------
