module Program.Logging.Interpreter(interpretIO) where
import Program.Logging.Types
import PivotalTracker.Story(getStory)
import Git.Actions(gitPull, gitShow, gitClone)
import System.Directory(doesDirectoryExist, getHomeDirectory, createDirectoryIfMissing)
import System.Environment(getEnv)
import Program.Types
import Control.Monad.Free(Free(..))
import Program.Types(Interaction)
import Data.Functor.Coproduct(Coproduct(..))
import Control.Monad.Trans.Either(EitherT)
import Control.Monad.Trans(liftIO)
import qualified Program.Interpreter as PI

type EIO = EitherT String IO
interpretIO :: Free (Coproduct LogF Interaction) a -> EIO a
interpretIO (Pure a) = return a
interpretIO (Free f) =
  case getCoproduct f of
    Left (Debug f)  -> do
      liftIO $ putStrLn "Debuggin"
      interpretIO f
    Right (GetEnv s fn)                 ->  liftIO (getEnv s) >>= interpretIO . fn
    Right (PrintF s n)                  ->  liftIO (putStrLn s) >> interpretIO n
    Right (GetHomeDir f)                ->  liftIO getHomeDirectory >>= interpretIO . f
    Right (CreateDirIfMissing b dir n)  ->  liftIO (createDirectoryIfMissing b dir) >> interpretIO n
    Right (DoesDirectoryExist file f)   ->  liftIO (doesDirectoryExist file) >>= interpretIO . f
    Right (GitPull path n)              ->  gitPull path >> interpretIO n
    Right (GitClone path repoName n)    ->  gitClone path repoName >> interpretIO n
    Right (GetStory token storyId fn)   ->  getStory token storyId >>= interpretIO . fn
    Right (GitShow options fn)          ->  gitShow options >>= interpretIO . fn
