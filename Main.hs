{-# LANGUAGE OverloadedStrings #-}
import Data.Char(toLower)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.Reader(runReaderT, ReaderT, ask)
import Control.Applicative((<$>))
import Data.Time.Format     (parseTime, formatTime)
import Data.Time.Clock      (UTCTime)
import Data.Time.Format(defaultTimeLocale)
import System.Console.ANSI
import qualified Network.HTTP.Types as NHT
import Control.Exception as E
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import qualified Data.ByteString.Char8 as BCH
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Lens (_String, key)
import Network.Wreq
import System.Directory(getHomeDirectory, createDirectoryIfMissing, renameDirectory, doesDirectoryExist)
import System.Environment
import Control.Lens
import Control.Monad((<=<))
import qualified Data.Maybe as MB
import qualified Text.Regex as TR
import Control.Monad(liftM)
import qualified Data.List as DL
import qualified Data.Text as T
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(..))
import qualified Control.Monad.Parallel as MP

type GitUrl = String
type HerokuFolderPath = String
type CommitMessage = String
type SHA = String
type StoryId = String
type EnvironmentName = String

data Environment = Environment { environmentName :: EnvironmentName, lastCommitter :: String, recentStories :: [PivotalStory] }
data PivotalStory = PivotalStory { pivotalStoryStatus :: T.Text, storyUpdatedAt :: Maybe UTCTime } deriving Show

instance Show Environment where
 show (Environment name lastCommitter stories)
    | all storyAccepted stories = colorGreen $ name ++ ": " ++ read lastCommitter ++ storyStatuses stories
    | otherwise                 = colorRed $ name ++ ": " ++ read lastCommitter ++ storyStatuses stories

lowerString :: [Char] -> [Char]
lowerString = map toLower

colorGreen string = setSGRCode [SetColor Foreground Dull Green] ++ string ++ "\x1b[0m"
colorRed string = setSGRCode [SetColor Foreground Dull Red] ++ string ++ "\x1b[0m"

storyStatuses :: [PivotalStory] -> String
storyStatuses xs = let dateString = if (length pendingAcceptance > 0)  then
                                      "since " ++ formattedLastUpdatedDate
                                      else "" in
                    " " ++ show (length pendingAcceptance) ++ " stories pending acceptance "  ++ dateString where
  formattedLastUpdatedDate = MB.maybe "" id $ formatTime defaultTimeLocale "%D" <$> lastUpdatedDate
  lastUpdatedDate = MB.listToMaybe . reverse . DL.sort  . MB.catMaybes $ map storyUpdatedAt pendingAcceptance
  pendingAcceptance = filter (not . storyAccepted) xs


gitUrlFor :: String -> GitUrl
gitUrlFor envName = "git@heroku.com:zephyr-" ++ lowerString envName ++ ".git"

deprecatedHerokuFolderPath = "/Users/gust/workspace/heroku_envs/"

mkDir folderName = readProcessWithExitCode "mkdir" [folderName] ""

fileNameForEnv :: String -> String
fileNameForEnv name = "zephyr-" ++ lowerString name

pullRepo :: EnvironmentName -> ReaderT HerokuFolderPath IO ExitCode
pullRepo name = do
  herokuFolderPath <- ask
  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", herokuFolderPath ++ (fileNameForEnv name), "pull", "-s", "ours"] ""
  return exitStatus

cloneRepo :: EnvironmentName -> ReaderT HerokuFolderPath IO ()
cloneRepo name = do
  let url = gitUrlFor name
  herokuFolderPath <- ask
  liftIO $ putStrLn $ "Creating a local copy of: " ++ name ++ " in " ++ herokuFolderPath ++ fileNameForEnv name ++ " this might take a minute"

  (exitStatus, _, _) <- liftIO $ readProcessWithExitCode "git" ["-C", herokuFolderPath, "clone", url] ""
  case exitStatus of 
    ExitSuccess -> return()
    ExitFailure x -> liftIO . putStrLn $ "Cloning repo " ++ name ++ " failed! This is probably because you don't have access to: " ++ url ++ " git repository. Give yoself access and this'll work"

hasLocalCopyOfRepo :: String -> ReaderT HerokuFolderPath IO Bool
hasLocalCopyOfRepo name = do
  herokuFolderPath <- ask
  (_, localDirContents, _) <- liftIO $ readProcessWithExitCode "ls" [herokuFolderPath] ""
  return $ T.isInfixOf (T.pack $ fileNameForEnv name) (T.pack localDirContents)

checkRepo name = do
  herokuFolderPath <- ask
  isTrue <- hasLocalCopyOfRepo name
  if isTrue then do
    pullStatus <- pullRepo name
    let fixMessage = "cd " ++ herokuFolderPath ++ " && git status\n" ++ "If there is a merge conflict abort the merge and try again. If there is uncommited work remove it and try again"
    case pullStatus of
      ExitSuccess -> checkEnvironmentStatus name
      ExitFailure x -> liftIO . putStrLn $ "Could not update local copy of: " ++ name ++ " pulling the environment failed. This is usually due to uncommited changes in that git directory. I don't know why this happens. To fix the is run\n"

  else cloneRepo name >> checkEnvironmentStatus name

lastCommitterName :: String -> ReaderT HerokuFolderPath IO String
lastCommitterName environment = do
  herokuFolderPath <- ask
  (_, name,_) <- liftIO $ readProcessWithExitCode "git" ["-C", herokuFolderPath ++ fileNameForEnv environment, "show", "--format=format:\"%an\"", "-s"] ""
  return name



textToDate  :: String -> Maybe UTCTime
textToDate  = parseTime defaultTimeLocale "%FT%X%QZ"

pivotalStories :: [StoryId] -> ReaderT HerokuFolderPath IO [PivotalStory]
pivotalStories storyIds = liftIO $ mapM getStory storyIds where
  getStory :: StoryId -> IO PivotalStory
  getStory storyId = do
    apiToken <- liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN"
    let options = defaults & header "X-TrackerToken" .~ [apiToken]
    res <- tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId)
    case res of
      Right response -> do
        let updatedAt = textToDate . T.unpack $ response ^. responseBody . key "updated_at" . _String
        let state =  response ^. responseBody . key "current_state" . _String
        return $ PivotalStory state updatedAt
      Left (StatusCodeException status headers _) -> do
        case NHT.statusCode status of
          -- We get a 403 when someone links to an epic
          403 -> return $ PivotalStory "invalid_story_id" Nothing
          404 -> return $ PivotalStory "invalid_story_id" Nothing
          statusCode   -> do
            putStrLn $ "Could not process request for story: " ++ storyId ++ ". openenvs received a " ++ show statusCode ++ " status from pivotal tracker. Defaulting to not accepted"
            return $ PivotalStory "not_accepted" Nothing
    where
      tryRequest :: IO a ->  IO (Either HttpException a)
      tryRequest = E.try

storyAccepted :: PivotalStory -> Bool
storyAccepted story = pivotalStoryStatus story == "accepted" || pivotalStoryStatus story == "invalid_story_id"

parseStoryIds :: EnvironmentName -> ReaderT HerokuFolderPath IO [StoryId]
parseStoryIds env = liftM storyIdsFromCommits commitMessages  where
  storyIdsFromCommits :: [CommitMessage] -> [StoryId]
  storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId)
    where
      parseStoryId :: CommitMessage -> Maybe [StoryId]
      parseStoryId = TR.matchRegex (TR.mkRegex "#([0-9]*)")

  commitMessages :: ReaderT HerokuFolderPath IO [CommitMessage]
  commitMessages = do
    messages <- mapM commitMessage [0..12]
    return $ messages
    where
      commitMessage :: Int -> ReaderT HerokuFolderPath IO String
      commitMessage commitNum = do
        herokuFolderPath <- ask
        (_, name,_) <- liftIO $ readProcessWithExitCode "git" ["-C", herokuFolderPath ++ fileNameForEnv env, "show", "HEAD~" ++ show commitNum, "--format=format:\"%s\"", "-s"] ""
        return name


analyzeCommits :: EnvironmentName -> ReaderT HerokuFolderPath IO Environment
analyzeCommits environment = do
  name <- lastCommitterName environment
  stories <- (pivotalStories <=< parseStoryIds) environment
  return $ Environment { lastCommitter = name, environmentName = environment, recentStories = stories }

environmentNames = [ "Alpha", "Bravo", "Echo", "Delta", "Foxtrot", "Juliet", "Romeo", "Tango", "Whiskey" ]

checkEnvironmentStatus :: EnvironmentName -> ReaderT HerokuFolderPath IO ()
checkEnvironmentStatus envName = do
  analyzeCommits envName >>= liftIO . putStrLn . show

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let herokuFolderPath = homeDir ++ "/heroku_envs/"
  hasDeprecatedFolderPath <- doesDirectoryExist deprecatedHerokuFolderPath
  createDirectoryIfMissing False herokuFolderPath
  if hasDeprecatedFolderPath then renameDirectory deprecatedHerokuFolderPath herokuFolderPath
                             else return ()
  flip runReaderT herokuFolderPath $ do
    liftIO $ putStrLn "Checking environments"
    MP.mapM checkRepo environmentNames >> return ()
