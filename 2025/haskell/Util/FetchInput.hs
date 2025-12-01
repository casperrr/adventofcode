module Util.FetchInput where

import Configuration.Dotenv
import System.Environment (getEnv)
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

getAOCCookie :: IO String
getAOCCookie = do
    loadFile defaultConfig
    getEnv "AOC_SESSION"

fetchAOC :: String -> IO (Response ByteString)
fetchAOC url = do
    cookie <- getAOCCookie
    initReq <- parseRequest url
    let req = addRequestHeader hCookie (BS.pack $ "session=" ++ cookie) initReq
    httpBS req

fetchAOCBody :: String -> IO ByteString
fetchAOCBody url = getResponseBody <$> fetchAOC url