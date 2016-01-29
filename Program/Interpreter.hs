module Program.Interpreter where
import Control.Applicative((<$>))
import PivotalTracker.Story(getStory)
import Git.Actions(gitPull, gitShow, gitClone)
import Control.Monad.Trans.Either(EitherT)
import Control.Monad.Trans(liftIO)
import Program.Types

import System.Directory(doesDirectoryExist, getHomeDirectory, createDirectoryIfMissing)
import Control.Monad.Free(Free(..), liftF)
import System.Environment(getEnv)


interpretIO :: Program a -> EitherT String IO a
interpretIO (Free (GetEnv s fn))                 =  (liftIO $ getEnv s) >>= interpretIO . fn
interpretIO (Free (PrintF s n))                  =  (liftIO $ putStrLn s) >> interpretIO n
interpretIO (Free (GetHomeDir f))                =  (liftIO getHomeDirectory) >>= interpretIO . f
interpretIO (Free (CreateDirIfMissing b dir n))  =  (liftIO $ createDirectoryIfMissing b dir) >> interpretIO n
interpretIO (Free (DoesDirectoryExist file f))        =  (liftIO $ doesDirectoryExist file) >>= interpretIO . f
interpretIO (Pure a)                             =  return a
interpretIO (Free (GitPull path n))              =  gitPull path >> interpretIO n
interpretIO (Free (GitClone path repoName n))    =  gitClone path repoName >> interpretIO n
interpretIO (Free (GetStory token storyId fn))         =  getStory token storyId >>= interpretIO . fn
interpretIO (Free (GitShow options fn))         =  gitShow options >>= interpretIO . fn

