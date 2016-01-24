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

doesFileExist' :: String -> Program Bool
doesFileExist' fileName = liftF $ DoesFileExist fileName id

getHomeDir' :: Program String
getHomeDir' = liftF $ GetHomeDir id
