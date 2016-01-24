module Program.Interpreter where
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode (..))
import Control.Monad.Trans.Either(right, left, runEitherT, EitherT)
import Control.Monad.Trans(liftIO)
import Program.Types

import System.Directory(doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import Control.Monad.Free(Free(..), liftF)
import System.Environment(getEnv)

type EIO = EitherT String IO

interpretIO :: Program a -> EIO a
interpretIO (Free (GetEnv s fn))                 =  (liftIO $ getEnv s) >>= interpretIO . fn
interpretIO (Free (PrintF s n))                  =  (liftIO $ print s) >> interpretIO n
interpretIO (Free (GetHomeDir f))                =  (liftIO getHomeDirectory) >>= interpretIO . f
interpretIO (Free (CreateDirIfMissing b dir n))  =  (liftIO $ createDirectoryIfMissing b dir) >> interpretIO n
interpretIO (Free (DoesFileExist file f))        =  (liftIO $ doesFileExist file) >>= interpretIO . f
interpretIO (Pure a)                             =  return a
interpretIO (Free (GitPull path n))              =  gitPull path >> interpretIO n
interpretIO (Free (GitClone path repoName n))    =  gitClone path repoName >> interpretIO n
interpretIO (Free (GetStory token storyId fn))         =  getStory token storyId >>= interpretIO . fn

getStory :: String -> StoryId -> EIO PivotalStory
getStory storyId = undefined 
  {- apiToken <- liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN" -}
  {- let options = defaults & header "X-TrackerToken" .~ [apiToken] -}
  {- res <- tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId) -}
  {- case res of -}
    {- Right response -> do -}
      {- let updatedAt = textToDate . T.unpack $ response ^. responseBody . key "updated_at" . _String -}
      {- let state =  response ^. responseBody . key "current_state" . _String -}
      {- return $ PivotalStory state updatedAt -}
    {- Left (StatusCodeException status headers _) -> do -}
      {- case NHT.statusCode status of -}
        {- -- We get a 403 when someone links to an epic -}
        {- 403 -> return $ PivotalStory "invalid_story_id" Nothing -}
        {- 404 -> return $ PivotalStory "invalid_story_id" Nothing -}
        {- statusCode   -> do -}
          {- putStrLn $ "Could not process request for story: " ++ storyId ++ ". openenvs received a " ++ show statusCode ++ " status from pivotal tracker. Defaulting to not accepted" -}
          {- return $ PivotalStory "not_accepted" Nothing -}
  {- where -}
    {- tryRequest :: IO a ->  IO (Either HttpException a) -}
    {- tryRequest = E.try -}

gitClone :: String -> String -> EIO ()
gitClone path url = do
  liftIO $ print "cloning"
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", path, "clone", url] ""
  case exitStatus of
    ExitSuccess -> right ()
    _           -> left "err"

gitPull :: String -> EIO ()
gitPull repoPath = do
  liftIO $ print $ "pulling " ++ repoPath
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", repoPath, "pull", "-s", "ours", "--rebase"] ""
  case exitStatus of
    ExitSuccess        -> right ()
    (ExitFailure a)    -> left $ "Error pulling status code: " ++ show a
