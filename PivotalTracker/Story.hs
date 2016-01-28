module PivotalTracker.Story(storyIdsFromCommits) where
import qualified Data.List as DL
import qualified Text.Regex as TR
import qualified Data.Maybe as MB
import Data.Maybe(Maybe)

storyIdsFromCommits :: [String] -> [[Int]]
storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId) where
  parseStoryId :: String -> Maybe [[Int]]
  parseStoryId commitMessage = (fmap read) <$> TR.matchRegex (TR.mkRegex "#([0-9]+)") commitMessage
