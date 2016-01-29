module Git.Actions where
import System.Process(readProcessWithExitCode)
import Control.Monad.Trans(liftIO)
import System.Exit(ExitCode (..))
import Git.Types
import Control.Monad.Trans.Either(right, left, runEitherT, EitherT)
import Data.List(intercalate)

type EIO = EitherT String IO

gitPull :: String -> EIO ()
gitPull repoPath = do
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", repoPath, "pull", "-s", "ours", "--rebase"] ""
  case exitStatus of
    ExitSuccess        -> right ()
    (ExitFailure a)    -> left $ "Error pulling status code: " ++ show a

gitShow :: [GitOption] -> EIO String
gitShow options = do
  (exitCode, commitMessage, _) <- liftIO $ readProcessWithExitCode "git" (optionsToCliArguments (Show:options)) ""
  case exitCode of
    ExitSuccess          -> right commitMessage
    ExitFailure status   -> left $ "Failed to run command git show " ++ (intercalate " " $ optionsToCliArguments options)  ++ " status was: " ++ show status

gitClone :: String -> String -> EIO ()
gitClone path url = do
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", path, "clone", url] ""
  case exitStatus of
    ExitSuccess -> right ()
    _           -> left "error Cloning"

