{-# LANGUAGE OverloadedStrings #-}

module Feed
( Item(..)
, Feed(..)
, feedToJSON
) where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BSL

data Item = Item { title        :: String
                 , url          :: String
                 , id           :: Maybe Int
                 , commentCount :: Maybe Int
                 , points       :: Maybe Int
                 , postedAgo    :: String
                 , author       :: String
                 } deriving (Show)

data Feed = Feed { items       :: [Item]
                 } deriving (Show)

instance ToJSON Feed where
 toJSON (Feed items) =
    object [ "items" .= items ]

instance ToJSON Item where
 toJSON (Item title url id commentCount points postedAgo author) =
    object [ "title"        .= title
           , "url"          .= url
           , "id"           .= id
           , "commentCount" .= commentCount
           , "points"       .= points
           , "postedAgo"    .= postedAgo
           , "author"       .= author
           ]

feedToJSON :: Feed -> BSL.ByteString
feedToJSON = encode
