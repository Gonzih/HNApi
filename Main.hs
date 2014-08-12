{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Data.IORef
import Control.Monad.IO.Class
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Parser

updateRefWithJson :: IORef T.Text -> IO ()
updateRefWithJson cache = do
    jsonBS <- fetchAndParse
    let encodedJsonData = TE.decodeUtf8 jsonBS
    _ <- atomicModifyIORef' cache (encodedJsonData,)

    putStrLn "Data was fetched"

updateLoop :: IORef T.Text -> IO ()
updateLoop ref = do
    updateRefWithJson ref
    threadDelay 300000000
    updateLoop ref
    return ()

main :: IO ()
main = scotty 4000 $ do
    middleware logStdoutDev

    cache <- liftIO $ newIORef T.empty

    _ <- liftIO $ forkIO $ updateLoop cache

    get "/" $ do
        jsonText <- liftIO $ readIORef cache
        text jsonText
        setHeader "content-type" "application/json"
