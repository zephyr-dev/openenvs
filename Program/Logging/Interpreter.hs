module Program.Logging.Interpreter(interpretIO) where
import Program.Logging.Types
import PivotalTracker.Story(getStory)
import Git.Actions(gitPull, gitShow, gitClone)
import System.Directory(doesDirectoryExist, getHomeDirectory, createDirectoryIfMissing)
import System.Environment(getEnv)
import Program.Types
import Control.Monad.Free(Free(..))
import Data.Functor.Coproduct(Coproduct(..))
import Control.Monad.Trans.Either(EitherT)
import Control.Monad.Trans(liftIO)

type EIO = EitherT String IO
interpretIO :: Show a => Free (Coproduct LogF Interaction) a -> EIO a
interpretIO (Pure a) = return a
interpretIO (Free f) =
  case getCoproduct f of
    Left (Debug s next)  -> do
      liftIO $ putStrLn s
      interpretIO next
    Right (GetEnv s fn)                 ->  liftIO (getEnv s) >>= interpretIO . fn
    Right (PrintF s n)                  ->  liftIO (putStrLn s) >> interpretIO n
    Right (GetHomeDir fn)                ->  liftIO getHomeDirectory >>= interpretIO . fn
    Right (CreateDirIfMissing b dir n)  ->  liftIO (createDirectoryIfMissing b dir) >> interpretIO n
    Right (DoesDirectoryExist file fn)   ->  liftIO (doesDirectoryExist file) >>= interpretIO . fn
    Right (GitPull path n)              ->  gitPull path >> interpretIO n
    Right (GitClone path repoName n)    ->  gitClone path repoName >> interpretIO n
    Right (GetStory token storyId fn)   ->  getStory token storyId >>= interpretIO . fn
    Right (GitShow options fn)          ->  gitShow options >>= interpretIO . fn
