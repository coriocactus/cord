{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad as Monad
import qualified Data.Aeson as JSON
import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.IORef as IOR
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Time.Format.ISO8601 as ISO8601
import qualified Data.Word as Word
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Environment as Env
import qualified Text.Blaze.Html.Renderer.Utf8 as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

application :: IOR.IORef [Project] -> Wai.Application
application projectsRef request respond = do
  _ <- updateAllLastModified projectsRef
  body <- Wai.lazyRequestBody request
  let request_method = BS.unpack $ Wai.requestMethod request
      request_path = BS.unpack $ Wai.rawPathInfo request
  case (request_method, request_path) of
    ("GET", "/") -> do
      response <- rootTemplateRoute projectsRef request
      respond response
    ("GET", "/test/states") -> do
      response <- statesRoute projectsRef request
      respond response
    ("POST", "/updated") -> do
      response <- updatedRoute projectsRef request body
      respond response
    _ -> respond $ notFoundTemplateRoute request

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

rootTemplateRoute :: IOR.IORef [Project] -> Wai.Request -> IO Wai.Response
rootTemplateRoute projectsRef _ = do
  projects <- IOR.readIORef projectsRef
  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml $ rootTemplate projects)

statesRoute :: IOR.IORef [Project] -> Wai.Request -> IO Wai.Response
statesRoute projectsRef _ = do
  projects <- IOR.readIORef projectsRef
  return $ Wai.responseLBS
    HTTP.status200
    [(HTTP.hContentType, "text/plain")]
    (BSL.pack $ unlines $ map show projects)

updatedRoute :: IOR.IORef [Project] -> Wai.Request -> BSL.ByteString
             -> IO Wai.Response
updatedRoute projectsRef request body = do
  maybeSecret <- Env.lookupEnv "HOOKER"
  let secret = maybe "" id maybeSecret
      maybeNotification = getNotification body
      signature = lookup "hooker-signature-256" $ Wai.requestHeaders request
      eitherVerification = verifySignature body signature secret
  statusCode <-
    case (maybeNotification, eitherVerification) of
      (Just notification, Right _) -> do
        let repo = sourceRepo notification
            version = sourceVersion notification
        _ <- updateRepoLastModified
              projectsRef
              repo
              (Version version)
        return HTTP.status200
      (_, _) -> return HTTP.status401
  return $ Wai.responseLBS
    statusCode
    [(HTTP.hContentType, "text/plain")]
    (BSL.pack "notification processed")

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

cordCSS :: H.Html
cordCSS = H.style $ H.text $ T.unwords
  [ ":root { color-scheme: light dark }"
  , "body, html { font-family: 'Lucida Console', monospace }"
  ]

rootTemplate :: [Project] -> H.Html
rootTemplate projects = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.title "cord"
    cordCSS
  H.body $ do
    H.h2 "projects"
    mapM_ projectTemplate projects

projectTemplate :: Project -> H.Html
projectTemplate p = H.div $ do
  H.string $ showStatus (pStatus p)
  H.string "["
  H.a H.! A.href (H.toValue $ pSource p) $
    H.string $ showVersion $ pVersion p
  H.string "]"
  H.a H.! A.href (H.toValue $ pURL p) $ H.string (pName p)

showStatus :: ProjectStatus -> String
showStatus Released = "RELEASED"
showStatus Building = "BUILDING"
showStatus Planning = "PLANNING"

showVersion :: Maybe Version -> String
showVersion (Just v) = "v" ++ unVersion v
showVersion Nothing = "source"

notFoundTemplateRoute :: Wai.Request -> Wai.Response
notFoundTemplateRoute _ = Wai.responseLBS
  HTTP.status404
  [(Headers.hContentType, BS.pack "text/html")]
  (R.renderHtml notFoundTemplate)

notFoundTemplate :: H.Html
notFoundTemplate = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.title "error"
    cordCSS
  H.body $ do
    H.p "404 - not found"
    H.p $ H.a H.! A.href "/" $ "home"

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

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
    HTTP.addRequestHeader "User-Agent" "joshwongcc" request
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
        "%Y.%m.%d"
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

updateAllLastModified :: IOR.IORef [Project] -> IO ()
updateAllLastModified projectsRef = do
  currentProjects <- IOR.readIORef projectsRef
  updatedProjects <- Monad.forM currentProjects $ \p -> do
    case pVersion p of
      Nothing -> do
        liveVersion <- getUpdatedAt $ pSource p
        case liveVersion of
          "-unavailable" -> return $ p { pVersion = Nothing }
          _ -> return $ p { pVersion = Just $ Version liveVersion }
      Just _ -> return p
  IOR.atomicWriteIORef projectsRef updatedProjects

updateRepoLastModified :: IOR.IORef [Project] -> String -> Version -> IO ()
updateRepoLastModified projectsRef repo version = do
  currentProjects <- IOR.readIORef projectsRef
  updatedProjects <- Monad.forM currentProjects $ \p -> do
    case repo == pSource p of
      True -> return $ p { pVersion = Just version }
      False -> return p
  IOR.atomicWriteIORef projectsRef updatedProjects

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

data Notification = Notification
  { sourceRepo :: String, sourceVersion :: String }
  deriving (Show)
instance JSON.FromJSON Notification where
  parseJSON = JSON.withObject "Notification" $ \v -> Notification
    <$> v JSON..: "repo"
    <*> v JSON..: "version"

getNotification :: BSL.ByteString -> Maybe Notification
getNotification body =
  case JSON.eitherDecode body of
    Right notification -> Just notification
    _ -> Nothing

verifySignature :: BSL.ByteString -> Maybe BS.ByteString -> String
                -> Either T.Text ()
verifySignature body signature secret = do
  case signature of
    Nothing -> Left "missing signature headers"
    Just digest -> do
      let packedSecret = BSL.pack secret
          hmacInstance = SHA.hmacSha256 packedSecret body
          expected = BS.pack $ SHA.showDigest $ hmacInstance
          actual = TE.encodeUtf8 $ T.drop 7 $ TE.decodeUtf8 digest
      if constantTimeCompare expected actual
        then Right ()
        else Left "signatures do not match"

constantTimeCompare :: BS.ByteString -> BS.ByteString -> Bool
constantTimeCompare a b =
  BS.length a == BS.length b &&
    0 == foldl'
      (\acc (x, y) -> acc Bits..|. Bits.xor x y)
      (0 :: Word.Word8) (B.zip a b)

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

data ProjectStatus = Released | Building | Planning
  deriving (Eq, Show, Enum, Bounded)

newtype Version = Version { unVersion :: String }
  deriving (Show)

data Project = Project
  { pName :: String
  , pURL :: String
  , pSource :: String
  , pStatus :: ProjectStatus
  , pVersion :: Maybe Version
  } deriving (Show)

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

projectsConfig :: Bool -> [Project]
projectsConfig prod =
  let version = if prod then Nothing else Just $ Version "local"
  in  [ Project "hooks" "https://github.com/joshwongcc/hooks"
          "https://github.com/joshwongcc/hooks"
          Released version
      , Project "cord" "https://github.com/joshwongcc/cord"
          "https://github.com/joshwongcc/cord"
          Released version
      , Project "claudia.vim" "https://github.com/joshwongcc/claudia.vim"
          "https://github.com/joshwongcc/claudia.vim"
          Released version
      , Project "anorby" "https://github.com/joshwongcc/anorby"
          "https://github.com/joshwongcc/anorby"
          Building version
      ]

--CRD-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D

main :: IO ()
main = do
  maybePort <- Env.lookupEnv "PORT"
  maybeProd <- Env.lookupEnv "PROD"
  let port = maybe 5000 read maybePort
      isProd = maybe False (== "1") maybeProd
  putStrLn $ "Server starting on port " ++ show port
  cStates <- IOR.newIORef $ projectsConfig isProd
  Warp.run port $ application cStates
