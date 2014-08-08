{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.UTF8 as C
import Data.List
import Data.List.Split (chunksOf)
import Data.Char (isSpace)

-- fetchNumber :: String -> String
-- fetchNumber = takeWhile (/= " ")

cleanUpInfo :: [String] -> [String]
cleanUpInfo = filter (/= " by ") . filter (/= "")

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

cleanUpPostedAgo :: String -> String
cleanUpPostedAgo = trim . takeWhile (/= '|')

cleanUpId :: String -> String
cleanUpId = tail . dropWhile (/= '=')

takeNumber :: String -> String
takeNumber = takeWhile (/= ' ')

main :: IO ()
main = do
    html <- simpleHttp "https://news.ycombinator.com/"
    urls <- runX $ doc html >>> urlAndTitle
    info <- runX $ doc html >>> infoSel

    mapM_ print $ init $ chunksOf 2 urls
    mapM_ print $ chunksOf 5 info
    print $ length $ init $ chunksOf 2 urls
    print $ length $ chunksOf 5 info

    where doc               = parseHtml . C.toString

          getTextOrEmpty    = getText `orElse` constA ""
          -- here there should be ifA used in case there is no link
          getByUrl urlInfix = css "a" >>> hasAttrValue "href" (isInfixOf urlInfix) /> getTextOrEmpty

          url               = getAttrValue "href"
          title             = getChildren >>> getText
          urlAndTitle       = css "td.title" >>> css "a" >>> url <+> title

          points            = css "span" /> getTextOrEmpty >>. map takeNumber
          author            = getByUrl "user"
          postedAgo         = getChildren >>> getTextOrEmpty >>. cleanUpInfo >>. map cleanUpPostedAgo
          comments          = getByUrl "item" >>. map takeNumber
          itemId            = css "a" >>> hasAttrValue "href" (isInfixOf "item") >>> getAttrValue "href" `orElse` constA "" >>. map cleanUpId
          infoSel           = css "td.subtext" >>> points <+> author <+> postedAgo <+> comments <+> itemId
