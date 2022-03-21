{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main where

import           Control.Concurrent.Async
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens          (_String, key)
import           Data.Bits                (Bits (xor))
import           Data.Data                (typeOf)
import           Data.Function
import           Data.List
import           Data.Map                 as Map
import qualified Data.Text.Internal       as L
import           Network.HTTP.Simple      ()
import           Network.Wreq
import qualified Network.Wreq.Session     as S

data Item =
  Item
    { by    :: String
    , title :: Maybe String
    , kids  :: Maybe [Int]
    }
  deriving (Show)

instance FromJSON Item where
  parseJSON (Object v) = do
    by <- v .: "by"
    title <- v .:? "title"
    kids <- v .:? "kids"
    return (Item {by = by, title = title, kids = kids})

type TopStoriesResponse = Response [Int]

type ItemReponse = Response Item

-- | Get the 30 top stories from Hacker News
getTop30Stories :: IO [Int]
getTop30Stories = do
  r <-
    asJSON =<< get "https://hacker-news.firebaseio.com/v0/topstories.json" :: IO TopStoriesResponse
  let body = Prelude.take 30 (r ^. responseBody)
  return body

-- | Generates an URL to get an item from its ID
generateUrl :: Int -> String
generateUrl x =
  "https://hacker-news.firebaseio.com/v0/item/" ++ show x ++ ".json"

-- | Returns an item from an ID
getOneItemFromId :: Int -> IO Item
getOneItemFromId x = do
  r <- asJSON =<< get (generateUrl x) :: IO ItemReponse
  let body = r ^. responseBody
  return body

-- | Returns the commentor of an Item from an ID
getByFromId :: Int -> IO L.Text
getByFromId x = do
  r <- get (generateUrl x)
  let body = r ^. responseBody . key "by" . _String
  return body

-- | Returns a list of all the titles from a list of items
getTitlesFromItems :: [Item] -> [String]
getTitlesFromItems []                         = []
getTitlesFromItems (Item _ (Just title) _:xs) = title : getTitlesFromItems xs
getTitlesFromItems (Item _ Nothing _:xs)      = getTitlesFromItems xs

-- | Returns a list of all the kids of from a list of Item
getKidsFromItems :: [Item] -> [Int]
getKidsFromItems []                        = []
getKidsFromItems (Item _ _ (Just kids):xs) = kids ++ getKidsFromItems xs
getKidsFromItems (Item _ _ Nothing:xs)     = getKidsFromItems xs

-- | Returns the most common element from a list
mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

-- MAIN
main :: IO ()
main = do
  print "Hello, here are the 30 top stories from Hacker News"
  listOfIds <- getTop30Stories
  listofItem <- mapConcurrently getOneItemFromId listOfIds
  print (getTitlesFromItems listofItem)
  listOfAllCommentors <-
    mapConcurrently getByFromId (getKidsFromItems listofItem)
  print (listOfAllCommentors)
