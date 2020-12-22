{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Lens ((^.), (^?))
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, Result (..), ToJSON, fromJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Aeson.Lens (key)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.Wreq as W
import System.Environment (getEnv)
import Web.Scotty (Param, get, json, params, scotty)

data Odds = Odds
  { _sportKey :: String,
    _sportNice :: String,
    _teams :: [String],
    _commenceTime :: Integer,
    _homeTeam :: String,
    _sites :: [BookData],
    _sitesCount :: Integer
  }
  deriving (Show)

instance FromJSON Odds where
  parseJSON =
    withObject
      "Odds"
      ( \obj -> do
          _sportKey <- obj .: "sport_key"
          _sportNice <- obj .: "sport_nice"
          _teams <- obj .: "teams"
          _commenceTime <- obj .: "commence_time"
          _homeTeam <- obj .: "home_team"
          _sites <- obj .: "sites"
          _sitesCount <- obj .: "sites_count"
          pure $ Odds {_sportKey, _sportNice, _teams, _commenceTime, _homeTeam, _sites, _sitesCount}
      )

instance ToJSON Odds where
  toJSON Odds {_sportKey, _sportNice, _teams, _commenceTime, _homeTeam, _sitesCount, _sites} =
    object
      [ "sport_key" .= _sportKey,
        "sport_nice" .= _sportNice,
        "teams" .= _teams,
        "commence_time" .= _commenceTime,
        "home_team" .= _homeTeam,
        "sites" .= _sites,
        "sites_count" .= _sitesCount
      ]

data Market = H2H [Double] | H2H_LAY [Double] | Spreads [Double] | Totals [Double] deriving (Show)

instance ToJSON Market where
  toJSON (Spreads a) = object ["totals" .= a]
  toJSON (Totals a) = object ["spreads" .= a]
  toJSON (H2H a) = object ["h2h" .= a]
  toJSON (H2H_LAY a) = object ["h2h_lay" .= a]

instance FromJSON Market where
  parseJSON =
    withObject
      "Market"
      ( \obj -> do
          arr <- case HM.keys obj of
            ["h2h"] -> obj .: "h2h"
            ["h2h_lay"] -> obj .: "h2h_lay"
            ["spreads"] -> obj .: "spreads"
            ["totals"] -> obj .: "totals"
            _ -> pure []
          pure $ case HM.keys obj of
            ["h2h"] -> H2H arr
            ["h2h_lay"] -> H2H_LAY arr
            ["spreads"] -> Spreads arr
            ["totals"] -> Totals arr
            _ -> H2H arr
      )

data BookData = BookData
  { _siteKey :: String,
    _siteNice :: String,
    _lastUpdate :: Integer,
    _odds :: Market
  }
  deriving (Show)

instance ToJSON BookData where
  toJSON BookData {_siteKey, _siteNice, _lastUpdate, _odds} =
    object
      [ "site_key" .= _siteKey,
        "site_nice" .= _siteNice,
        "last_update" .= _lastUpdate,
        "odds" .= _odds
      ]

instance FromJSON BookData where
  parseJSON =
    withObject
      "BookData"
      ( \obj -> do
          _siteKey <- obj .: "site_key"
          _siteNice <- obj .: "site_nice"
          _lastUpdate <- obj .: "last_update"
          _odds <- obj .: "odds"
          pure $ BookData {_siteKey, _siteNice, _lastUpdate, _odds}
      )

data Sports = Sports
  { _key :: String,
    _active :: Bool,
    _group :: String,
    _details :: String,
    _title :: String,
    _hasOutRights :: Bool
  }
  deriving (Generic, Show, Eq)

instance ToJSON Sports where
  toJSON Sports {_key, _active, _group, _details, _title, _hasOutRights} =
    object
      [ "key" .= _key,
        "active" .= _active,
        "group" .= _group,
        "details" .= _details,
        "title" .= _title,
        "has_outrights" .= _hasOutRights
      ]

instance FromJSON Sports where
  parseJSON =
    withObject
      "Sports"
      ( \obj -> do
          _key <- obj .: "key"
          _active <- obj .: "active"
          _group <- obj .: "group"
          _details <- obj .: "details"
          _title <- obj .: "title"
          _hasOutRights <- obj .: "has_outrights"
          pure $ Sports {_key, _active, _group, _details, _title, _hasOutRights}
      )

data Sport = CFB | NFL | MLB | MMA

data Region = AU | UK | EU | US

data OddsFormat = American | Decimal deriving (Show)

regionToOddsFormat :: Region -> OddsFormat
regionToOddsFormat US = American
regionToOddsFormat _ = Decimal

oddsFormatToString :: OddsFormat -> String
oddsFormatToString American = "american"
oddsFormatToString _ = "decimal"

marketToString :: Market -> String
marketToString m = case m of
  Spreads _ -> "spreads"
  H2H _ -> "h2h"
  H2H_LAY _ -> "h2h"
  Totals _ -> "totals"

-- stringToMarket :: String -> Market
-- stringToMarket "spreads" = Spreads []
-- stringToMarket "h2h" = H2H []
-- stringToMarket "h2h_lay" = H2H_LAY []
-- stringToMarket "totals" = Totals []
-- stringToMarket _ = H2H []

stringToRegion :: T.Text -> Region
stringToRegion "au" = AU
stringToRegion "UK" = UK
stringToRegion "US" = US
stringToRegion "EU" = EU
stringToRegion _ = US

regionToKey :: Region -> String
regionToKey a = case a of
  AU -> "au"
  UK -> "uk"
  US -> "us"
  EU -> "eu"

sportToKey :: Sport -> String
sportToKey sport = case sport of
  CFB -> "americanfootball_ncaaf"
  NFL -> "americanfootball_nfl"
  MLB -> "baseball_mlb"
  MMA -> "mma_mixed_martial_arts"

keyToSport :: T.Text -> Sport
keyToSport "americanfootball_ncaaf" = CFB
keyToSport "americanfootball_nfl" = NFL
keyToSport "baseball_mlb" = MLB
keyToSport "mma_mixed_martial_arts" = MMA
keyToSport "" = MMA

baseUrl :: String
baseUrl = "https://api.the-odds-api.com"

oddsApi :: String -> Sport -> Region -> Market -> String
oddsApi key s r m = "/v3/odds/?apiKey=" ++ key ++ "&sport=" ++ sport ++ "&region=" ++ region ++ "&mkt=" ++ mkt ++ "&oddsFormat=" ++ odds
  where
    sport = sportToKey s
    odds = (oddsFormatToString . regionToOddsFormat) r
    region = regionToKey r
    mkt = marketToString m

sportsApi :: String -> String
sportsApi key = "/v3/sports/?apiKey=" ++ key

createUrl :: String -> String
createUrl route = baseUrl ++ route

loadEnv :: IO String
loadEnv = do
  loadFile defaultConfig
  getEnv "odds_api"

getResultFromMaybe :: Maybe (Result [Sports]) -> [Sports]
getResultFromMaybe a = case a of
  Just x -> case x of
    Success val -> val
    Error _ -> []
  Nothing -> []

oddsFromMaybe :: Maybe (Result [Odds]) -> [Odds]
oddsFromMaybe a = case a of
  Just x -> case x of
    Success val -> val
    Error _ -> []
  Nothing -> []

getSportsData :: IO [Sports]
getSportsData = do
  env <- loadEnv
  let url = createUrl $ sportsApi env
  resp <- W.get url
  let body = resp ^. W.responseBody
  let dat = body ^? key "data"
  let x = fromJSON <$> dat
  (pure . getResultFromMaybe) x

getOddsData :: Sport -> Region -> Market -> IO [Odds]
getOddsData sport region market = do
  apiKey <- loadEnv
  let url = createUrl $ oddsApi apiKey sport region market
  print url
  resp <- W.get url
  let body = resp ^. W.responseBody
  let dat = body ^? key "data"
  print dat
  let x = fromJSON <$> dat
  print x
  (pure . oddsFromMaybe) x

-- [("sport","nfl"),("mkt","us")]
paramsToRequest :: [Param] -> (Sport, Region)
paramsToRequest [(_, spt), (_, cntry)] = (keyToSport spt, stringToRegion cntry)

main :: IO ()
main = scotty 3000 $ do
  get "/getpicks" $ do
    a <- liftIO getSportsData
    json a

  get "/getOdds/:mkt/:sport" $ do
    -- param sport needs to be key from getSportsData
    p <- params
    let (spt, rgn) = paramsToRequest p
    d <- liftIO $ getOddsData spt rgn (H2H [])
    json d

-- liftIO $ print d
-- json d
