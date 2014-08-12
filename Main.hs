{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Server where

import Data.IORef
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T

import Web.Scotty

import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = scotty 4000 $ do
    middleware logStdoutDev

    cache <- liftIO $ newIORef T.empty

    post "/" $ do
        value <- param "value"
        liftIO $ atomicModifyIORef' cache (, value)
        text value

    get "/" $ do
        responseData <- liftIO $ readIORef cache
        text responseData
        setHeader "content-type" "application/json"
