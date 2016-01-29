{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module Program.Types where
import qualified Data.Maybe as MB
import Git.Types(GitOption)
import qualified Data.List as DL
import Data.Time.Format     (formatTime)
import Data.Time.Format(defaultTimeLocale)
import Control.Monad.Free(Free(..))
import System.Console.ANSI(setSGRCode, SGR(..), ConsoleLayer(Foreground), ColorIntensity(Dull), Color(Green, Red))
import Data.Text(Text)
import Data.Time.Clock      (UTCTime)
import PivotalTracker.Story(PivotalStory(..), storyAccepted)
type Program = Free Interaction

data Interaction next =
  GetEnv String (String -> next) |
  PrintF String next |
  GetHomeDir (String -> next) |
  DoesDirectoryExist String (Bool -> next) |
  CreateDirIfMissing Bool String next |
  GetStory String String (PivotalStory -> next) |
  GitPull String next |
  GitShow [GitOption] (String -> next) |
  GitClone String String next   deriving (Functor)


data Environment = Environment {
  environmentName :: String,
  lastCommitter :: String,
  recentStories :: [PivotalStory]
}

colorGreen :: String -> String
colorGreen string = setSGRCode [SetColor Foreground Dull Green] ++ string ++ resetCode
colorRed :: String -> String
colorRed string = setSGRCode [SetColor Foreground Dull Red] ++ string ++ resetCode

resetCode = "\x1b[0m"

instance Show Environment where
 show (Environment name lastCommitter stories)
    | all storyAccepted stories = colorGreen $ name ++ ": " ++ lastCommitter ++ storyStatuses stories
    | otherwise                 = colorRed $ name ++ ": " ++ lastCommitter ++ storyStatuses stories

storyStatuses :: [PivotalStory] -> String
storyStatuses xs = let dateString = if (length pendingAcceptance > 0)  then
                                      "since " ++ formattedLastUpdatedDate
                                      else "" in
                    " " ++ show (length pendingAcceptance) ++ " stories pending acceptance "  ++ dateString where
  formattedLastUpdatedDate = MB.maybe "" id $ formatTime defaultTimeLocale "%D" <$> lastUpdatedDate
  lastUpdatedDate = MB.listToMaybe . reverse . DL.sort  . MB.catMaybes $ map storyUpdatedAt pendingAcceptance
  pendingAcceptance = filter (not . storyAccepted) xs

