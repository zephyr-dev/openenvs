{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Free(Free(..), liftF)
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import Data.Aeson.Lens (_String, key)
import Control.Applicative((<$>))
import Data.List(splitAt)
import System.Environment(getEnv)
import Network.Wreq
import Control.Lens
import Data.ByteString.Char8(pack)
import Data.Text(unpack)

import qualified Data.Text as T
import Control.Exception as E
data PivotalStory = PivotalStory { pivotalStoryStatus :: String } deriving Show

{- getStory :: String -> IO PivotalStory -}
{- getStory storyId = do -}
  {- token <- pack <$> getEnv "PIVOTAL_TRACKER_API_TOKEN" -}
  {- let options = defaults & header "X-TrackerToken" .~ [token] -}
  {- res <- E.try (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId) -}
  {- case res of -}
    {- Right response -> do -}
      {- let story = PivotalStory $ unpack $ response ^. responseBody . key "current_state" . _String -}
      {- print story -}
      {- return story -}
    {- Left (StatusCodeException status headers _) -> do -}
      {- putStrLn $ "Could not process request for story: " ++ storyId ++ ". openenvs received a " ++ show status -}
      {- return $ PivotalStory "not_status" -}

main :: IO ()
main = do
  interpretIO $ do
    apiToken <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
    storyId <- getLine'
    getStory apiToken storyId
  return ()


interpretIO :: Free Interaction a -> IO ()
interpretIO (Free (GetEnv s fn)) = getEnv s >>= interpretIO . fn
interpretIO (Free (GetLine fn)) = getLine >>= interpretIO . fn
interpretIO (Free (PrintF s n)) = print s >> interpretIO n
interpretIO (Pure _)          = return ()

type TrackerApi = Free Interaction
data Interaction next = GetEnv String (String -> next) | GetLine (String -> next) | PrintF String next

getEnv' :: String -> Free Interaction String
getEnv' key = liftF $ GetEnv key id

getLine' :: Free Interaction String
getLine' = liftF $ GetLine id


instance Functor Interaction where
  fmap f (GetEnv s n) = GetEnv s (fmap f n)
  fmap f (GetLine n) = GetLine (fmap f n)
  fmap f (PrintF s n) = PrintF s (f n)


printF :: String -> Free Interaction ()
printF s = liftF $ PrintF s ()

getStory :: String -> String -> TrackerApi ()
getStory apiToken storyId = do
                  printF apiToken
                  printF storyId
  {- let options = defaults & header "X-TrackerToken" .~ [token] -}
  {- res <- E.try (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId) -}
  {- case res of -}
    {- Right response -> do -}
      {- let story = PivotalStory $ unpack $ response ^. responseBody . key "current_state" . _String -}
      {- print story -}
      {- return story -}
    {- Left (StatusCodeException status headers _) -> do -}
      {- putStrLn $ "Could not process request for story: " ++ storyId ++ ". openenvs received a " ++ show status -}
      {- return $ PivotalStory "not_status" -}
