{-# LANGUAGE OverloadedStrings #-}

module HackerNews
  ( getAllInfo
  ) where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Lens             ((^.))
import           Data.Aeson               (FromJSON (parseJSON), Value (Object),
                                           (.:?))
import           Data.Aeson.Lens          (_String, key)
import           Data.Data                (typeOf)
import           Data.Function            (on)
import           Data.List                as DL (filter, group, map, sort,
                                                 sortBy, take)
import qualified Data.Text.Internal       as L
import           Data.Time                (diffUTCTime, getCurrentTime)
import           Network.Wreq             (Response, asJSON, get, responseBody)
import qualified Network.Wreq.Session     as S

-- | An item can either be a top story or a comment here
data Item =
  Item
    { idt   :: Maybe Int
    , by    :: Maybe String
    , title :: Maybe String
    , kids  :: Maybe [Int]
    }
  deriving (Show)

instance Eq Item where
  (Item id1 _ _ _) == (Item id2 _ _ _) = id1 == id2

instance Ord Item where
  (Item id1 _ _ _) `compare` (Item id2 _ _ _) = id1 `compare` id2

instance FromJSON Item where
  parseJSON (Object v) = do
    idt <- v .:? "id"
    by <- v .:? "by"
    title <- v .:? "title"
    kids <- v .:? "kids"
    return (Item {idt = idt, by = by, title = title, kids = kids})

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

-- | Returns an item (story or comment) from an ID
getOneItemFromId :: Int -> IO Item
getOneItemFromId x = do
  r <- asJSON =<< get (generateUrl x) :: IO ItemReponse
  let body = r ^. responseBody
  return body

-- | Returns a list of all the titles ('title' in the result JSON) from a list of items
getTitlesFromItems :: [Item] -> [String]
getTitlesFromItems []                           = []
getTitlesFromItems (Item _ _ (Just title) _:xs) = title : getTitlesFromItems xs
getTitlesFromItems (Item _ _ Nothing _:xs)      = getTitlesFromItems xs

-- | Returns a list of all the comments ('kids' in the result JSON) of from a list of Item
getCommentsFromItems :: [Item] -> [[Int]]
getCommentsFromItems [] = []
getCommentsFromItems (Item _ _ _ (Just comments):xs) =
  comments : getCommentsFromItems xs
getCommentsFromItems (Item _ _ _ Nothing:xs) = getCommentsFromItems xs

-- | Returns a list of all the authors from a list of Items
getAuthorsFromComments :: [Item] -> [String]
getAuthorsFromComments [] = []
getAuthorsFromComments (Item _ (Just author) _ _:xs) =
  author : getAuthorsFromComments xs
getAuthorsFromComments (Item _ Nothing _ _:xs) = getAuthorsFromComments xs

-- | Returns all the comments ('kids' from the top story) from a list of ID
getCommentsFromId :: [Int] -> IO [Item]
getCommentsFromId = mapConcurrently getOneItemFromId

-- | Returns a list of tuple consisting of (element, occurence)
mostCommon :: Ord a => [a] -> [(a, Int)]
mostCommon = DL.map (\x -> (head x, length x)) . group . sort

-- | Returns the top 10 elements appearing the most in a list based on their occurence
sortMostCommon :: Ord a => [a] -> [(a, Int)]
sortMostCommon x = DL.take 10 (sortBy (flip compare `on` snd) (mostCommon x))

-- | Returns the commentors names and occurence, this function also returns values in double, you need getCommentorAndFinalOccurence to remove them
getTopCommentorAndOccurencePerStory ::
     Eq a => [(a, Int)] -> [(a, Int)] -> [(a, [(a, Int)])]
getTopCommentorAndOccurencePerStory y [] = []
getTopCommentorAndOccurencePerStory y (a:xs) =
  (fst a, DL.filter ((== fst a) . fst) y) :
  getTopCommentorAndOccurencePerStory xs y

-- | Returns the commentors names and occurence in all comments recovered, removes double values from getTopCommentorAndOccurencePerStory
getCommentorAndFinalOccurence :: [(String, [(String, Int)])] -> [(String, Int)]
getCommentorAndFinalOccurence [] = []
getCommentorAndFinalOccurence ((_, []):xs) = getCommentorAndFinalOccurence xs
getCommentorAndFinalOccurence ((x, (y, z):zs):xs) =
  (x, z) : getCommentorAndFinalOccurence xs

getAllInfo :: IO ()
getAllInfo = do
  startTime <- getCurrentTime
  print "Hello"
  listOfIds <- getTop30Stories
  top30Stories <- mapConcurrently getOneItemFromId listOfIds
  print "30 top stories have been recovered from Hacker News"
  let listOfTitles = getTitlesFromItems top30Stories
  -- List of direct commentors ID (commented directly on the story)
  let listOfFirstCommentorsId = getCommentsFromItems top30Stories
  -- List of direct commentors (commented directly on the story)
  listOfFirstCommentors <-
    mapConcurrently getCommentsFromId listOfFirstCommentorsId
  -- List of sub commentors ID (commented on a comment)
  let listOfSubCommentorsId = DL.map getCommentsFromItems listOfFirstCommentors
  -- List of sub commentors (commented on a comment)
  listOfSubCommentors <-
    mapConcurrently getCommentsFromId (concat listOfSubCommentorsId)
  -- Concatenation of direct and sub commenters (does not take into account sub commentors of sub commentors)
  let listOfAllComments =
        DL.map
          getAuthorsFromComments
          (listOfFirstCommentors ++ listOfSubCommentors)
  let numberOfCommentsPerAuthor = mostCommon (concat listOfAllComments)
  let topCommentor = DL.map sortMostCommon listOfAllComments
  let topCommentorsWithOccurence =
        DL.map
          (getCommentorAndFinalOccurence .
           getTopCommentorAndOccurencePerStory numberOfCommentsPerAuthor)
          topCommentor
  let topCommentorPerStory = zip listOfTitles topCommentorsWithOccurence
  print
    "Here are the 30 top stories with their top commenters along with their total number of comments on thos 30 stories"
  print topCommentorPerStory
  endTime <- getCurrentTime
  putStr
    ("It took " ++
     show (diffUTCTime endTime startTime) ++ " to get the results. Goodbye.")
