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
import           Data.List                as DL
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
  let body = Prelude.take 3 (r ^. responseBody)
  return body

-- | Generates an URL to get an item from its ID
generateUrl :: Int -> String
generateUrl x =
  "https://hacker-news.firebaseio.com/v0/item/" ++ show x ++ ".json"

-- | Returns an item (story or comment) from an ID
getOneItemFromId :: Int -> IO Item
getOneItemFromId x = do
  r <- asJSON =<< get (generateUrl x) :: IO ItemReponse
  let body = r ^. responseBody
  return body

-- | Returns the author ('by' in the result JSON) of an Item from an ID
getAuthorFromId :: Int -> IO L.Text
getAuthorFromId x = do
  r <- get (generateUrl x)
  let body = r ^. responseBody . key "by" . _String
  return body

-- | Returns a list of all the titles ('title' in the result JSON) from a list of items
getTitlesFromItems :: [Item] -> [String]
getTitlesFromItems []                         = []
getTitlesFromItems (Item _ (Just title) _:xs) = title : getTitlesFromItems xs
getTitlesFromItems (Item _ Nothing _:xs)      = getTitlesFromItems xs

-- | Returns a list of all the comments ('kids' in the result JSON) of from a list of Item
getCommentsFromItems :: [Item] -> [[Int]]
getCommentsFromItems [] = []
getCommentsFromItems (Item _ _ (Just comments):xs) =
  comments : getCommentsFromItems xs
getCommentsFromItems (Item _ _ Nothing:xs) = getCommentsFromItems xs

-- | Returns all the authors ('by' in the result JSON) from a list of ID
getAuthorsFromIds :: [Int] -> IO [L.Text]
getAuthorsFromIds = mapConcurrently getAuthorFromId

-- | Returns a list of tuple consisting of (element, occurence)
mostCommon :: Ord a => [a] -> [(a, Int)]
mostCommon = DL.map (\x -> (head x, length x)) . group . sort

-- | Returns the top 10 elements appearing the most in a list based on their occurence
sortMostCommon :: Ord a => [a] -> [(a, Int)]
sortMostCommon x = DL.take 10 (sortBy (flip compare `on` fst) (mostCommon x))

-- | Creates a list of tuples from two lists
createTuppleFromTwoLists :: [a] -> [b] -> [(a, b)]
createTuppleFromTwoLists [] _          = []
createTuppleFromTwoLists _ []          = []
createTuppleFromTwoLists (x:xs) (y:ys) = (x, y) : createTuppleFromTwoLists xs ys

-- MAIN
main :: IO ()
main = do
  print "Hello, here are the 30 top stories from Hacker News"
  listOfIds <- getTop30Stories
  top30Stories <- mapConcurrently getOneItemFromId listOfIds
  let listOfTitles = getTitlesFromItems top30Stories
  print listOfTitles
  let sublistsOfComments = getCommentsFromItems top30Stories
  listOfAllCommentors <- mapConcurrently getAuthorsFromIds sublistsOfComments
  let topCommentor = DL.map sortMostCommon listOfAllCommentors
  let finalResult = createTuppleFromTwoLists listOfTitles topCommentor
  print finalResult
