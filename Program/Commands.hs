module Program.Commands where
import Control.Monad.Free(liftF, Free(..))
import Program.Types

gitPull' :: String -> Program ()
gitPull' path = liftF $ GitPull path ()

gitClone' :: String -> String -> Program ()
gitClone' path repoName = liftF $ GitClone path repoName ()

print' :: String -> Program ()
print' s = liftF $ PrintF s ()

getEnv' :: String -> Program String
getEnv' key = liftF $ GetEnv key id

createDirectoryIfMissing' :: Bool -> String -> Program ()
createDirectoryIfMissing' bool dir = liftF $ CreateDirIfMissing bool dir ()

doesDirectoryExist' :: String -> Program Bool
doesDirectoryExist' fileName = liftF $ DoesDirectoryExist fileName id

getHomeDir' :: Program String
getHomeDir' = liftF $ GetHomeDir id

parseCommitLog :: String -> (String, String) -> Program [StoryId]
parseCommitLog path (name, _) = liftF $ ParseCommitLog (path ++ name) id

getPivotalStories :: String -> [StoryId] -> Program [PivotalStory]
getPivotalStories token storyIds = mapM getStory storyIds
  where
    getStory :: StoryId -> Program PivotalStory
    getStory storyId = liftF $ GetStory token storyId id
