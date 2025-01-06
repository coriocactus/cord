{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment as Env

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Bits as Bits
import qualified Data.Word as Word
import qualified Data.IORef as IOR
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Aeson as JSON

import qualified Control.Monad as Monad

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as R

import qualified SourceMeta as Source

application :: IOR.IORef [WhatIf] -> Wai.Application
application statesRef request respond = do
    body <- Wai.lazyRequestBody request
    let request_method = BS.unpack $ Wai.requestMethod request
        request_path = BS.unpack $ Wai.rawPathInfo request
    case (request_method, request_path) of
        ("GET", "/") -> do
            response <- rootTemplateRoute statesRef request
            respond response
        ("GET", "/test/states") -> do
            response <- statesRoute statesRef request
            respond response
        ("POST", "/updated") -> do
            response <- updatedRoute statesRef request body
            respond response
        _ -> respond $ notFoundTemplateRoute request

statefulMiddleware :: IOR.IORef [WhatIf]
                   -> (IOR.IORef [WhatIf] -> t1 -> t2 -> IO b)
                   -> t1 -> t2 -> IO b
statefulMiddleware statesRef app request respond = do
    _ <- updateAllLastModified statesRef
    app statesRef request respond

updateAllLastModified :: IOR.IORef [WhatIf] -> IO ()
updateAllLastModified statesRef = do
    currentConfig <- IOR.readIORef statesRef
    updatedConfig <- Monad.forM currentConfig $ \w -> do
        case wLastModified w of
            Nothing -> do
                liveLastMod <- Source.getUpdatedAt $ unSource $ wSource w
                case liveLastMod of 
                    "-unavailable" -> return $ w
                        { wLastModified = Nothing
                        }
                    _ -> return $ w
                        { wLastModified = Just $ WhatIfLastModified liveLastMod
                        }
            Just _ -> return w
    IOR.atomicWriteIORef statesRef updatedConfig

updateRepoLastModified :: IOR.IORef [WhatIf]
                       -> WhatIfSource
                       -> WhatIfLastModified
                       -> IO ()
updateRepoLastModified statesRef repo lastMod = do
    currentConfig <- IOR.readIORef statesRef
    updatedConfig <- Monad.forM currentConfig $ \w -> do
        case repo == wSource w of
            True -> return $ w { wLastModified = Just lastMod }
            False -> return w
    IOR.atomicWriteIORef statesRef updatedConfig

monolith :: IOR.IORef [WhatIf] -> Wai.Application
monolith statesRef = Mid.logStdout $ statefulMiddleware statesRef application

rootTemplateRoute :: IOR.IORef [WhatIf] -> Wai.Request -> IO Wai.Response
rootTemplateRoute statesRef _ = do
    states <- IOR.readIORef statesRef
    return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $ rootTemplate states)

rootTemplate :: [WhatIf] -> H.Html
rootTemplate states = H.docTypeHtml $ H.html $ do
    H.head $ do
        H.title "eta: 0 mins"
        H.style $ H.text fullCSS
    H.body $ do
        H.span H.! A.id "top" $ ""
        H.div H.! A.id "frame" $ do
            H.h1 "internet common #5819574234"
            H.div $ do
                H.a H.! A.href "#what-if" $ "what if ..."
        H.span H.! A.id "what-if" $ ""
        H.div H.! A.id "frame" $ do
            H.h1 "what if ..."
            mkWhatIfsHtml states

mkWhatIfsHtml :: [WhatIf] -> H.Html
mkWhatIfsHtml [] = ""
mkWhatIfsHtml (x:[]) = whatIfTemplate x
mkWhatIfsHtml (x:xs) = whatIfTemplate x >> H.hr >> mkWhatIfsHtml xs

whatIfTemplate :: WhatIf -> H.Html
whatIfTemplate w = case wStatus w of 
    Released -> H.div $ do
        H.p H.! A.class_ "what-if-q" $ do
            H.a H.! A.href (H.toValue $ unURL $ wUrl w) $
                H.i $ H.string (unQuestion $ wQuestion w)
        H.p H.! A.class_ "what-if-s" $ do
            H.a H.! A.href (H.toValue $ unSource $ wSource w) $
                H.string $ showWhatIfLastModified $ wLastModified w
    WIP -> H.div $ do
        H.p H.! A.class_ "what-if-q" $ do
            H.i $ H.string $ "( " ++ (unQuestion $ wQuestion w) ++ " )"
        H.p H.! A.class_ "what-if-s" $ do
            H.a H.! A.href (H.toValue $ unSource $ wSource w) $
                H.string $ showWhatIfLastModified $ wLastModified w
    Announced -> H.div $ do
        H.p H.! A.class_ "what-if-q" $ do
            H.i $ H.string $ "( " ++ (unQuestion $ wQuestion w) ++ " )"

showWhatIfLastModified :: Maybe WhatIfLastModified -> String
showWhatIfLastModified m =
    case m of 
      Just l -> "[ v" ++ unLastModified l ++ " ]"
      Nothing -> "[ source ]"

statesRoute :: IOR.IORef [WhatIf] -> Wai.Request -> IO Wai.Response
statesRoute statesRef _ = do
    states <- IOR.readIORef statesRef
    return $ Wai.responseLBS
        HTTP.status200
        [(HTTP.hContentType, "text/plain")]
        (BSL.pack $ unlines $ map show states)

updatedRoute :: IOR.IORef [WhatIf]
             -> Wai.Request
             -> BSL.ByteString
             -> IO Wai.Response
updatedRoute statesRef request body = do
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
                    statesRef
                    (WhatIfSource repo)
                    (WhatIfLastModified version)
              return HTTP.status200
          (_, _) -> return HTTP.status401
    return $ Wai.responseLBS
        statusCode
        [(HTTP.hContentType, "text/plain")]
        (BSL.pack "notification processed")

notFoundTemplateRoute :: Wai.Request -> Wai.Response
notFoundTemplateRoute _ = Wai.responseLBS
    HTTP.status404
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml notFoundTemplate)

notFoundTemplate :: H.Html
notFoundTemplate = H.docTypeHtml $ H.html $ do
    H.head $ do
        H.title "error"
        H.style $ I.preEscapedText fullCSS
    H.body $ do
        H.div H.! A.id "frame" $ do
            H.h1 "404 - not found"
            H.h1 $ do
               H.a H.! A.class_ "link" H.! A.href "/" $ "home"

-- ---------------------------------------------------------------------------

cssEntry :: T.Text -> [T.Text] -> T.Text
cssEntry selector properties = T.unlines
    [ selector <> " {"
    , T.unlines (map (\p -> "    " <> p <> ";") properties)
    , "}"
    ]

cssProperty :: T.Text -> T.Text -> T.Text
cssProperty property value = T.intercalate ": " [property, value]

combineCSS :: [T.Text] -> T.Text
combineCSS = T.concat

rootCSS :: T.Text
rootCSS = cssEntry ":root" 
    [ cssProperty "color-scheme" "light dark"
    ]

bodyHtmlCSS :: T.Text
bodyHtmlCSS = cssEntry "body, html"
    [ cssProperty "margin" "0 auto"
    , cssProperty "padding" "0 50px"
    , cssProperty "font-family" "'Lucida Console', monospace"
    , cssProperty "font-size" "20px"
    , cssProperty "max-width" "1280px"
    ]

frameCSS :: T.Text
frameCSS = cssEntry "#frame"
    [ cssProperty "min-height" "100vh"
    , cssProperty "min-height" "100dvh"
    , cssProperty "text-align" "center"
    , cssProperty "align-content" "center"
    , cssProperty "border" "3px"
    , cssProperty "box-sizing" "border-box"
    , cssProperty "background" "light-dark(white, black)"
    , cssProperty "color" "light-dark(black, white)"
    ]

linkCSS :: T.Text
linkCSS = cssEntry "a"
    [ cssProperty "text-decoration" "none"
    , cssProperty "color" "#4169e1"
    ]

hrCSS :: T.Text
hrCSS = cssEntry "hr"
    [ cssProperty "border" "none"
    , cssProperty "height" "2px"
    , cssProperty "background-color" "lightgrey"
    , cssProperty "margin" "30px auto"
    ]

whatIfQuestionCSS :: T.Text
whatIfQuestionCSS = cssEntry ".what-if-q"
    [ cssProperty "" ""
    , cssProperty "" ""
    ]

whatIfSourceCSS :: T.Text
whatIfSourceCSS = cssEntry ".what-if-s"
    [ cssProperty "font-family" "Optima, serif"
    , cssProperty "font-weight" "600"
    ]

fullCSS :: T.Text
fullCSS = combineCSS
    [ rootCSS
    , bodyHtmlCSS
    , frameCSS
    , linkCSS
    , hrCSS
    , whatIfQuestionCSS
    , whatIfSourceCSS
    ]

-- ---------------------------------------------------------------------------

verifySignature :: BSL.ByteString
                -> Maybe BS.ByteString
                -> String
                -> Either T.Text ()
verifySignature body signature secret = do
    case signature of
        Nothing -> Left "missing signature headers"
        Just digest -> do
            let
                packedSecret = BSL.pack secret
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

-- ---------------------------------------------------------------------------

newtype WhatIfQuestion = WhatIfQuestion { unQuestion :: String }
    deriving (Show)
newtype WhatIfURL = WhatIfURL { unURL :: String }
    deriving (Show)
newtype WhatIfSource = WhatIfSource { unSource :: String }
    deriving (Show, Eq)
data WhatIfProjectStatus = Released | WIP | Announced
    deriving (Eq, Show, Enum, Bounded)
data WhatIfLastModified = WhatIfLastModified { unLastModified :: String }
    deriving (Show)

data WhatIf = WhatIf 
    { wQuestion :: WhatIfQuestion
    , wUrl :: WhatIfURL
    , wSource :: WhatIfSource
    , wStatus :: WhatIfProjectStatus
    , wLastModified :: Maybe WhatIfLastModified
    } deriving (Show)

-- ---------------------------------------------------------------------------

data Notification = Notification 
    { sourceRepo :: String, sourceVersion :: String }
    deriving (Show)
instance JSON.FromJSON Notification where
    parseJSON = JSON.withObject "Notification" $ \v -> Notification
        <$> v JSON..: "repo"
        <*> v JSON..: "version"

getNotification :: BSL.ByteString -> Maybe Notification
getNotification body = case JSON.eitherDecode body of
                         Right notification -> Just notification
                         _ -> Nothing

-- ---------------------------------------------------------------------------

whatIfsConfig :: [WhatIf]
whatIfsConfig = 
    [ WhatIf
        (WhatIfQuestion "websites are cool again?")
        (WhatIfURL "https://www.cordcivilian.com")
        (WhatIfSource "https://github.com/cordcivilian/cord")
        (Released)
        (Nothing)
    , WhatIf
        (WhatIfQuestion "opinions have consequences?")
        (WhatIfURL "https://anorby.cordcivilian.com")
        (WhatIfSource "https://github.com/cordcivilian/anorby")
        (WIP)
        (Nothing)
    , WhatIf
        (WhatIfQuestion "habits ruled?")
        (WhatIfURL "https://batsch.cordcivilian.com")
        (WhatIfSource "https://github.com/cordcivilian/batsch")
        (WIP)
        (Nothing)
    ]

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    maybePort <- Env.lookupEnv "PORT"
    let autoPort = 5000
        port = maybe autoPort read maybePort
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    cStates <- IOR.newIORef whatIfsConfig
    Warp.run port $ monolith cStates
