{-# LANGUAGE OverloadedStrings #-}
module Program.Interpreter where
import Control.Applicative((<$>))
import System.Process(readProcessWithExitCode)
import qualified Data.Text as T
import Data.Time.Format     (parseTime)
import Data.Time.Format(defaultTimeLocale)
import Data.Time.Clock      (UTCTime)
import Network.Wreq(defaults, header, getWith, responseBody)
import Control.Exception as E
import qualified Network.HTTP.Types as NHT
import Control.Lens((&), (.~), (^.))
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import Data.Aeson.Lens (_String, key)
import System.Exit(ExitCode (..))
import Control.Monad.Trans.Either(right, left, runEitherT, EitherT)
import Control.Monad.Trans(liftIO)
import Data.List(intercalate)
import Program.Types
import qualified Data.ByteString.Char8 as BCH
import Program.Types.Git(GitOption, optionsToCliArguments)

import System.Directory(doesDirectoryExist, getHomeDirectory, createDirectoryIfMissing)
import Control.Monad.Free(Free(..), liftF)
import System.Environment(getEnv)

type EIO = EitherT String IO

interpretIO :: Program a -> EIO a
interpretIO (Free (GetEnv s fn))                 =  (liftIO $ getEnv s) >>= interpretIO . fn
interpretIO (Free (PrintF s n))                  =  (liftIO $ print s) >> interpretIO n
interpretIO (Free (GetHomeDir f))                =  (liftIO getHomeDirectory) >>= interpretIO . f
interpretIO (Free (CreateDirIfMissing b dir n))  =  (liftIO $ createDirectoryIfMissing b dir) >> interpretIO n
interpretIO (Free (DoesDirectoryExist file f))        =  (liftIO $ doesDirectoryExist file) >>= interpretIO . f
interpretIO (Pure a)                             =  return a
interpretIO (Free (GitPull path n))              =  gitPull path >> interpretIO n
interpretIO (Free (GitClone path repoName n))    =  gitClone path repoName >> interpretIO n
interpretIO (Free (GetStory token storyId fn))         =  getStory token storyId >>= interpretIO . fn
interpretIO (Free (GitShow options fn))         =  gitShow options >>= interpretIO . fn

textToDate  :: String -> Maybe UTCTime
textToDate  = parseTime defaultTimeLocale "%FT%X%QZ"

gitShow :: [GitOption] -> EIO String
gitShow options = do
  (exitCode, commitMessage, _) <- liftIO $ readProcessWithExitCode "git" (["show"] ++ (optionsToCliArguments options)) "" --, "-C", path, "HEAD~" ++ show commitNum, "--format=format:\"%s\"", "-s"] "" 
  case exitCode of
    ExitSuccess          -> right commitMessage
    ExitFailure status   -> left $ "Failed to run command git show " ++ (intercalate " " $ optionsToCliArguments options)  ++ " status was: " ++ show status

{- lastCommitterName :: String -> ReaderT HerokuFolderPath IO String -}
{- lastCommitterName environment = do -}
  {- herokuFolderPath <- ask -}
  {- (_, name,_) <- liftIO $ readProcessWithExitCode "git" ["-C", herokuFolderPath ++ fileNameForEnv environment, "show", "--format=format:\"%an\"", "-s"] "" -}

getStory :: String -> StoryId -> EIO PivotalStory
getStory token storyId = do
  let options = defaults & header "X-TrackerToken" .~ [BCH.pack token]
  res <- liftIO $ tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ show storyId)
  case res of
    Right response -> do
      let updatedAt = textToDate . T.unpack $ response ^. responseBody . key "updated_at" . _String
      let state =  response ^. responseBody . key "current_state" . _String
      return $ PivotalStory state updatedAt
    Left (StatusCodeException status headers _) -> do
      case NHT.statusCode status of
        403 -> return $ PivotalStory "invalid_story_id" Nothing
        404 -> return $ PivotalStory "invalid_story_id" Nothing
        statusCode   -> return $ PivotalStory "not_accepted" Nothing
  where
    tryRequest :: IO a ->  IO (Either HttpException a)
    tryRequest = E.try

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
