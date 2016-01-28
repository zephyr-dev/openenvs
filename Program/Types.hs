{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module Program.Types where
import qualified Data.Maybe as MB
import qualified Data.List as DL
import Data.Time.Format     (formatTime)
import Data.Time.Format(defaultTimeLocale)
import Control.Monad.Free(Free(..))
import System.Console.ANSI(setSGRCode, SGR(..), ConsoleLayer(Foreground), ColorIntensity(Dull), Color(Green, Red))
import Data.Text(Text)
import Data.Time.Clock      (UTCTime)
type Program = Free Interaction

data PivotalStory = PivotalStory {
  pivotalStoryStatus :: Text,
  storyUpdatedAt :: (Maybe UTCTime)
}

type StoryId = [Int]
data Environment = Environment {
  environmentName :: String,
  lastCommitter :: String,
  recentStories :: [PivotalStory]
}

storyAccepted :: PivotalStory -> Bool
storyAccepted story = pivotalStoryStatus story == "accepted" || pivotalStoryStatus story == "invalid_story_id"

colorGreen string = setSGRCode [SetColor Foreground Dull Green] ++ string ++ "\x1b[0m"
colorRed string = setSGRCode [SetColor Foreground Dull Red] ++ string ++ "\x1b[0m"

instance Show Environment where
 show (Environment name lastCommitter stories)
    | all storyAccepted stories = colorGreen $ name ++ ": " ++ read lastCommitter ++ storyStatuses stories
    | otherwise                 = colorRed $ name ++ ": " ++ read lastCommitter ++ storyStatuses stories

storyStatuses :: [PivotalStory] -> String
storyStatuses xs = let dateString = if (length pendingAcceptance > 0)  then
                                      "since " ++ formattedLastUpdatedDate
                                      else "" in
                    " " ++ show (length pendingAcceptance) ++ " stories pending acceptance "  ++ dateString where
  formattedLastUpdatedDate = MB.maybe "" id $ formatTime defaultTimeLocale "%D" <$> lastUpdatedDate
  lastUpdatedDate = MB.listToMaybe . reverse . DL.sort  . MB.catMaybes $ map storyUpdatedAt pendingAcceptance
  pendingAcceptance = filter (not . storyAccepted) xs

data Interaction next =
  GetEnv String (String -> next) |
  PrintF String next |
  GetHomeDir (String -> next) |
  DoesDirectoryExist String (Bool -> next) |
  CreateDirIfMissing Bool String next |
  GetStory String StoryId (PivotalStory -> next) |
  GitPull String next |
  GitShow String Int (String -> next) |
  GitClone String String next   deriving (Functor)

