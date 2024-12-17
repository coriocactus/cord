{-# LANGUAGE OverloadedStrings #-}

module SourceMeta where

import qualified Data.Aeson as JSON

import qualified Network.HTTP.Simple as HTTP

import qualified Data.Time as Time
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Time.Format.ISO8601 as ISO8601

data RepoInfo = RepoInfo
  { repoHtmlURL :: String
  , repoUpdatedAt :: String
  } deriving (Show)

instance JSON.FromJSON RepoInfo where
  parseJSON = JSON.withObject "RepoInfo" $ \v -> RepoInfo
    <$> v JSON..: "html_url"
    <*> v JSON..: "updated_at"

getRepoInfo :: String -> String -> IO (Either String RepoInfo)
getRepoInfo owner repo = do
  let url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo
  request <- HTTP.parseRequest url
  response <- HTTP.httpLBS $
    HTTP.addRequestHeader "User-Agent" "cordcivilian" request
  let jsonBody = HTTP.getResponseBody response
  return $ JSON.eitherDecode jsonBody

getUpdatedAt :: String -> IO String
getUpdatedAt s = do
    let (owner, repo) = getOwnerRepoFromSourceLink s
    result <- getRepoInfo owner repo
    case result of
        Right repoInfo -> do
            let maybeUpdatedAt = getVersion $ repoUpdatedAt repoInfo -- pure
            return $ maybe "-unavailable" id maybeUpdatedAt
        _ -> return "-unavailable"

getVersion :: String -> Maybe String
getVersion iso8601 = do
    utcTime <- ISO8601.iso8601ParseM iso8601 :: Maybe Time.UTCTime
    let hour =
          DateTimeFormat.formatTime
          DateTimeFormat.defaultTimeLocale
          "%H"
          utcTime
        hourlyVersion = ['a'..'z'] !! read hour
        dailyVersion =
          DateTimeFormat.formatTime
          DateTimeFormat.defaultTimeLocale
          "%Y-%m-%d"
          utcTime
        version = dailyVersion ++ [hourlyVersion]
    return version

getOwnerRepoFromSourceLink :: String -> (String, String)
getOwnerRepoFromSourceLink s = extractTuple $ split s '/'

extractTuple :: [String] -> (String, String)
extractTuple (_:_:_:owner:repo:_) = (owner, repo)
extractTuple _ = ("", "")

split :: String -> Char -> [String]
split str delim =
  case break (==delim) str of
    (a, _:b) -> a : split b delim
    (a, _)   -> [a]
