module PivotalTracker.Types where

import qualified Data.Text as T
import Data.Time.Clock      (UTCTime)
import Data.Maybe(Maybe)

data PivotalStory = PivotalStory {
  pivotalStoryStatus :: T.Text,
  storyUpdatedAt :: (Maybe UTCTime)
} 

