{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.UTF8 as C
import Data.List
import Data.Char (isSpace)

-- fetchNumber :: String -> String
-- fetchNumber = takeWhile (/= " ")

cleanUpInfo :: [String] -> [String]
cleanUpInfo = filter (/= " by ") . filter (/= "")

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

cleanUpPostedAgo :: [String] -> [String]
cleanUpPostedAgo = map $ trim . takeWhile (/= '|')

cleanUpId :: [String] -> [String]
cleanUpId = map $ tail . dropWhile (/= '=')

takeNumber :: [String] -> [String]
takeNumber = map $ takeWhile (/= ' ')

flattenPostsData :: ((String, String),
                     (String, (String, (String, (String, String)))))
                    -> [String]
flattenPostsData ((url, title),
                  (postedAgo, (points, (user, (comments, itemId))))) = [ url
                                                                       , title
                                                                       , points
                                                                       , user
                                                                       , postedAgo
                                                                       , comments
                                                                       , itemId]

main :: IO ()
main = do
    html <- simpleHttp "https://news.ycombinator.com/"
    urls <- runX $ doc html >>> urlAndTitle
    info <- runX $ doc html >>> infoSel

    let postsData = map flattenPostsData $ (init urls) `zip` info
    mapM_ print postsData

    where doc                   = parseHtml . C.toString

          -- helpers
          getByUrl urlInfix     = css "a" >>> hasAttrValue "href" (isInfixOf urlInfix)
          getTextByUrl urlInfix = getByUrl urlInfix /> getText

          -- first tr
          url                   = getAttrValue "href"
          title                 = getChildren >>> getText

          urlAndTitle           = css "td.title" >>> css "a" >>> (url &&& title)

          -- second tr
          postedAgo             = getChildren >>> getText >>. cleanUpInfo >>. cleanUpPostedAgo -- Always present

          points                = css "span" /> getText >>. takeNumber
          comments              = getTextByUrl "item" >>. takeNumber
          author                = getTextByUrl "user"
          itemId                = getByUrl "item" >>> getAttrValue "href" >>. cleanUpId
          emptyInfo             = constA ("",("",("","")))

                                                                              -- post on everything except postedAgo
          infoSel               = css "td.subtext" >>> postedAgo &&& ((points &&& author &&& comments &&& itemId) `orElse` emptyInfo)
