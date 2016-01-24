{-# LANGUAGE OverloadedStrings #-}
module Program.Interpreter where
import qualified Data.List as DL
import qualified Data.Maybe as MB
import Control.Applicative((<$>))
import System.Process(readProcessWithExitCode)
import qualified Text.Regex as TR
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
import Program.Types
import qualified Data.ByteString.Char8 as BCH

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
interpretIO (Free (ParseCommitLog repoPath fn))         =  parseCommitLog repoPath >>= interpretIO . fn

textToDate  :: String -> Maybe UTCTime
textToDate  = parseTime defaultTimeLocale "%FT%X%QZ"

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

parseCommitLog :: String -> EIO [StoryId]
parseCommitLog repoPath = storyIdsFromCommits <$> commitMessages repoPath
  where
    storyIdsFromCommits :: [String] -> [StoryId]
    storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId) where
      parseStoryId :: String -> Maybe [StoryId]
      parseStoryId commitMessage = (fmap read) <$> TR.matchRegex (TR.mkRegex "#([0-9]+)") commitMessage
    commitMessages :: String -> EIO [String]
    commitMessages repoPath = mapM (commitMessage repoPath) [0..12]
      where
        commitMessage :: String -> Int -> EIO String
        commitMessage repoPath commitNum = do
          (_, name,_) <- liftIO $ readProcessWithExitCode "git" ["-C", repoPath, "show", "HEAD~" ++ show commitNum, "--format=format:\"%s\"", "-s"] ""
          return name

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
