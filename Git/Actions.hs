module Git.Actions where
import System.Process(readProcessWithExitCode)
import Control.Monad.Trans(liftIO)
import System.Exit(ExitCode (..))
import Git.Types
import Control.Monad.Trans.Either(right, left, EitherT)

type EIO = EitherT String IO

--TODO: Move api of gitPull/gitClone to mimic gitShow by taking opts
gitPull :: String -> EIO ()
gitPull repoPath = do
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", repoPath, "pull", "-s", "ours", "--rebase"] ""
  case exitStatus of
    ExitSuccess        -> right ()
    ExitFailure a    -> left $ "Error pulling process exited with code: " ++ show a

gitShow :: [GitOption] -> EIO String
gitShow options = do
  let showOpts = showOptions options
  (exitCode, commitMessage, _) <- liftIO $ readProcessWithExitCode "git" (optionsToCliArguments showOpts) ""
  case exitCode of
    ExitSuccess          -> right commitMessage
    ExitFailure status   -> left $ "Failed to run command git show " ++ (optionsToString showOpts)  ++ " process exited with code " ++ show status

gitClone :: String -> String -> EIO ()
gitClone path url = do
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", path, "clone", url] ""
  case exitStatus of
    ExitSuccess        -> right ()
    ExitFailure status -> left $ "error Cloning process exited with code " ++ show status

