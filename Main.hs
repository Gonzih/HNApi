{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Data.IORef
import Control.Monad (forever, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import System.IO.Error (catchIOError)
import System.Environment (getEnv)
import System.IO (stderr, hPutStr, hPrint)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Parser

updateRefWithJson :: IORef T.Text -> IO ()
updateRefWithJson cache = do
    jsonBS <- fetchAndParse
    let encodedJsonData = TE.decodeUtf8 jsonBS
    _ <- atomicModifyIORef' cache (encodedJsonData,)

    putStrLn "Data was fetched"

logIOError :: IOError -> IO ()
logIOError e =
    hPutStr stderr "Error while updating cache: "
    >> hPrint stderr e

updateLoop :: IORef T.Text -> IO ()
updateLoop ref = forever $ do
    catchIOError (updateRefWithJson ref) logIOError
    threadDelay 300000000

getEnvVar :: String -> IO String
getEnvVar key = catchIOError (getEnv key) (\_ -> return "4000")

main :: IO ()
main = do
    port <- liftM read $ getEnvVar "PORT"
    scotty port $ do
        middleware logStdoutDev

        cache <- liftIO $ newIORef T.empty

        _ <- liftIO $ forkIO $ updateLoop cache

        get "/" $ do
            jsonText <- liftIO $ readIORef cache
            text jsonText
            setHeader "content-type" "application/json"
