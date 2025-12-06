module Util.FetchInput where

import Configuration.Dotenv ( loadFile, defaultConfig )
import System.Environment ( getEnv )
import Network.HTTP.Simple
    ( parseRequest,
      addRequestHeader,
      getResponseBody,
      httpBS,
      Response )
import Network.HTTP.Types.Header ( hCookie )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString ( ByteString )

getAOCCookie :: IO String
getAOCCookie = do
    loadFile defaultConfig
    getEnv "AOC_SESSION"

fetch :: String -> IO (Response ByteString)
fetch url = do
    cookie <- getAOCCookie
    initReq <- parseRequest url
    let req = addRequestHeader hCookie (BS.pack $ "session=" ++ cookie) initReq
    httpBS req

fetchBody :: String -> IO ByteString
fetchBody url = getResponseBody <$> fetch url

fetchBodyStr :: String -> IO String
fetchBodyStr url = BS.unpack . getResponseBody <$> fetch url

inputAOCURL :: Int -> String
inputAOCURL d = "https://adventofcode.com/2025/day/"++ show d ++ "/input"