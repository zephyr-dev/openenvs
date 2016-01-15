module Git.Commands where
import Control.Monad.Free(liftF, Free(..))
import Git.Types

gitPull' :: String -> Git ()
gitPull' path = liftF $ GitPull path ()

gitClone' :: String -> String -> Git ()
gitClone' path repoName = liftF $ GitClone path repoName ()
