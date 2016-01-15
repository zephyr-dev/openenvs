module Git.Interpreters.IO(interpret) where
import Control.Monad.Free(Free(..))
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode (..))
import Control.Monad.Trans.Either(right, left, runEitherT, EitherT)
import Control.Monad.Trans(liftIO)
import Git.Types

type EIO = EitherT String IO

interpret :: Git a -> EIO a
interpret (Free (GitPull path n))           = gitPull path >> interpret n
interpret (Free (GitClone path repoName n)) = gitClone path repoName >> interpret n
interpret (Pure a)                          = return a

gitClone :: String -> String -> EIO ()
gitClone path url = do 
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", path, "clone", url] ""
  case exitStatus of
    ExitSuccess -> right ()
    _           -> left "err"

gitPull :: String -> EIO ()
gitPull repoPath = do 
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", repoPath, "pull", "-s", "ours", "--rebase"] ""
  case exitStatus of
    ExitSuccess -> right ()
    _           -> left "err"
