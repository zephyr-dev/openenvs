{-# LANGUAGE DeriveFunctor     #-}
module Program.Types where
import Control.Monad.Free(Free(..))
import Data.Text(Text)
import Data.Time.Clock      (UTCTime)
type Program = Free Interaction
data PivotalStory = PivotalStory Text (Maybe UTCTime)
type StoryId = [Int]

data Interaction next =
  GetEnv String (String -> next) |
  PrintF String next |
  GetHomeDir (String -> next) |
  DoesDirectoryExist String (Bool -> next) |
  CreateDirIfMissing Bool String next |
  ParseCommitLog String ([StoryId] -> next) |
  GetStory String StoryId (PivotalStory -> next) |
  GitPull String next |
  GitClone String String next   deriving (Functor)

