{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Data.IORef
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Parser

main :: IO ()
main = scotty 4000 $ do
    middleware logStdoutDev

    cache <- liftIO $ newIORef T.empty

    post "/" $ do
        jsonBS <- liftIO $ fetchAndParse
        let encodedJsonData = TE.decodeUtf8 jsonBS

        _ <- liftIO $ putStrLn "Data was fetched"
        _ <- liftIO $ atomicModifyIORef' cache (, encodedJsonData)
        text encodedJsonData
        setHeader "content-type" "application/json"

    get "/" $ do
        jsonText <- liftIO $ readIORef cache
        text jsonText
        setHeader "content-type" "application/json"
