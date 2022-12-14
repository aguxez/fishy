-- Request weather from cities using the OpenWeather API
{-# LANGUAGE OverloadedStrings #-}

module Weather (weatherFrom, Temperature (..)) where

import Config (APIKey (..), FishyConfig (..))
import Data.Aeson (FromJSON (..), decode, withObject, (.:))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

data RequestType = Weather | Geo

data WeatherRequestBody = WeatherBody Text Float Float | GeoBody Text ByteString

data Temperature = Temperature {grades :: Float, feelsLike :: Float}

instance FromJSON Temperature where
  parseJSON = withObject "Temperature" $ \obj -> do
    main <- obj .: "main"
    Temperature <$> main .: "temp" <*> main .: "feels_like"

data LatLon = LatLon Float Float

instance FromJSON LatLon where
  parseJSON = withObject "LatLon" $ \obj -> LatLon <$> obj .: "lat" <*> obj .: "lon"

endpoint :: RequestType -> Request
endpoint reqType = parseRequest_ ("https://api.openweathermap.org" ++ rest reqType)
  where
    rest :: RequestType -> String
    rest Weather = "/data/2.5/weather"
    rest Geo = "/geo/1.0/direct"

weatherQueryString :: WeatherRequestBody -> [(ByteString, Maybe ByteString)]
weatherQueryString (GeoBody apiKey cityName) = [("q", Just cityName), ("appid", Just $ encodeUtf8 apiKey)]
weatherQueryString (WeatherBody apiKey lat lon) =
  [ ("lat", Just . encodeUtf8 . floatToText $ lat),
    ("lon", Just . encodeUtf8 . floatToText $ lon),
    ("appid", Just $ encodeUtf8 apiKey),
    ("units", Just "metric")
  ]

floatToText :: Float -> Text
floatToText = T.pack . show

findOpenWeatherApiKey :: FishyConfig -> Text
findOpenWeatherApiKey (FishyConfig apiKeys) = maybe "" apiKeyValue (findOpenWeatherApiKey' apiKeys)
  where
    findOpenWeatherApiKey' :: [APIKey] -> Maybe APIKey
    findOpenWeatherApiKey' = find (\v -> apiKeyName v == "open_weather")

handleGeoResponse :: Response BL.ByteString -> Either Text LatLon
handleGeoResponse geoRes = do
  case decodeGeo geoRes of
    Just [] -> Left "Could not find a location with that name..."
    Just (x : _) -> Right x
    Nothing -> Left "There was an error decoding the geolocation response..."
  where
    decodeGeo :: Response BL.ByteString -> Maybe [LatLon]
    decodeGeo = decode . responseBody

handleWeatherResponse :: Response BL.ByteString -> ByteString -> Either Text Text
handleWeatherResponse weatherRes city = do
  case decodeWeatherRes weatherRes of
    Just weather -> do
      Right $
        T.intercalate
          "\n"
          [ T.concat [T.toTitle (decodeUtf8 city), "\n"],
            T.concat ["Temperature: ", floatToText $ grades weather],
            T.concat ["Feels like: ", floatToText $ feelsLike weather]
          ]
    Nothing -> Left "There was an error decoding the temperature response..."
  where
    decodeWeatherRes :: Response BL.ByteString -> Maybe Temperature
    decodeWeatherRes = decode . responseBody

weatherFrom :: FishyConfig -> ByteString -> IO Text
weatherFrom config city = do
  manager <- newManager tlsManagerSettings
  let geoUrl = endpoint Geo
      weatherUrl = endpoint Weather
      apiKey = findOpenWeatherApiKey config
      geoReq = setQueryString (weatherQueryString (GeoBody apiKey city)) geoUrl

  geoRes <- httpLbs geoReq manager
  case handleGeoResponse geoRes of
    Right (LatLon lat lon) -> do
      let weatherReq = setQueryString (weatherQueryString (WeatherBody apiKey lat lon)) weatherUrl
      weatherRes <- httpLbs weatherReq manager

      return $ either id id (handleWeatherResponse weatherRes city)
    Left err -> return err
