module Program.Commands where
import Control.Monad.Free(liftF, Free(..))
import Git.Types(GitOption)
import Program.Types
import PivotalTracker.Types(PivotalStory(..))

gitPull' :: String -> Program ()
gitPull' path = liftF $ GitPull path ()

gitClone' :: String -> String -> Program ()
gitClone' path repoName = liftF $ GitClone path repoName ()

print' :: Show a => a -> Program ()
print' s = liftF $ PrintF (show s) ()

getEnv' :: String -> Program String
getEnv' key = liftF $ GetEnv key id

createDirectoryIfMissing' :: Bool -> String -> Program ()
createDirectoryIfMissing' bool dir = liftF $ CreateDirIfMissing bool dir ()

doesDirectoryExist' :: String -> Program Bool
doesDirectoryExist' fileName = liftF $ DoesDirectoryExist fileName id

getHomeDir' :: Program String
getHomeDir' = liftF $ GetHomeDir id

gitShow' :: [GitOption] -> Program String
gitShow' options = liftF $ GitShow options id

getPivotalStories :: String -> [String] -> Program [PivotalStory]
getPivotalStories token storyIds = mapM getStory storyIds
  where
    getStory :: String -> Program PivotalStory
    getStory storyId = liftF $ GetStory token storyId id
