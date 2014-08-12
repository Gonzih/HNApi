{-# LANGUAGE OverloadedStrings #-}

module Parser
( fetchAndParse )
where

import Data.List
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Text.Read (readMaybe)
import Network.HTTP.Conduit
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Feed

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

createItem :: ((String, String),
               (String, (String, (String, (String, String)))))
           -> Item
createItem ((pUrl, pTitle),
            (pPostedAgo, (pPoints, (pUser, (pComments, pItemId)))))
           = Item
             pTitle
             pUrl
             maybeId
             maybeComments
             maybePoints
             pPostedAgo
             pUser
             where maybeId       = readMaybe pItemId
                   maybeComments = readMaybe pComments
                   maybePoints   = readMaybe pPoints


createFeed :: [((String, String),
               (String, (String, (String, (String, String)))))]
           -> Feed
createFeed itemTuples = Feed $ map createItem itemTuples

fetchAndParse :: IO (BSL.ByteString)
fetchAndParse = do
    html <- simpleHttp "https://news.ycombinator.com/"
    urls <- runX $ doc html >>> urlAndTitleA
    info <- runX $ doc html >>> infoSelA

    let feed = createFeed $ init urls `zip` info

    return $ feedToJSON feed

    where doc                   = parseHtml . BSL.toString

          -- helpers
          getByUrl urlInfix     = css "a" >>> hasAttrValue "href" (isInfixOf urlInfix)
          getTextByUrl urlInfix = getByUrl urlInfix /> getText

          -- first tr
          urlA                  = getAttrValue "href"
          titleA                = getChildren >>> getText

          urlAndTitleA          = css "td.title" >>> css "a" >>> (urlA &&& titleA)

          -- second tr
          postedAgoA            = getChildren >>> getText >>. cleanUpInfo >>. cleanUpPostedAgo

          pointsA               = css "span" /> getText >>. takeNumber
          commentsA             = getTextByUrl "item" >>. takeNumber
          authorA               = getTextByUrl "user"
          itemIdA               = getByUrl "item" >>> getAttrValue "href" >>. cleanUpId
          emptyInfoA            = constA ("",("",("","")))

                                                                       -- fail on everything except postedAgo
          infoSelA              = css "td.subtext" >>> postedAgoA &&& ((pointsA &&& authorA &&& commentsA &&& itemIdA) `orElse` emptyInfoA)
