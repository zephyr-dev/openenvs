{-# LANGUAGE OverloadedStrings #-}
import Data.Char(toLower)
import System.Console.ANSI
import qualified Network.HTTP.Types as NHT
import Control.Exception as E
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import qualified Data.ByteString.Char8 as BCH
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Lens (_String, key)
import Network.Wreq
import System.Environment
import Control.Lens
import Control.Monad((<=<))
import qualified Data.Maybe as MB
import qualified Text.Regex as TR
import Control.Monad(liftM)
import qualified Data.List as DL
import qualified Data.Text as T
import System.Process(readProcessWithExitCode)
import qualified Control.Monad.Parallel as MP

type GitUrl = String
type SHA = String
type StoryId = String
type EnvironmentName = String

lowerString :: [Char] -> [Char]
lowerString = map toLower

data Environment = Environment { environmentName :: EnvironmentName, lastCommitter :: String, storiesAccepted :: Bool }

instance Show Environment where
 show (Environment name lastCommitter True) = setSGRCode [SetColor Foreground Dull Green] ++ name ++ ": " ++ read lastCommitter ++ "\x1b[0m"
 show (Environment name lastCommitter False) = setSGRCode [SetColor Foreground Dull Red] ++  name ++ ": " ++ read lastCommitter ++ "\x1b[0m"


gitUrlFor :: String -> GitUrl
gitUrlFor envName = "git@heroku.com:zephyr-" ++ lowerString envName ++ ".git"

herokuFolderName = "/Users/gust/workspace/heroku_envs/"

makeHerokuFolder = readProcessWithExitCode "mkdir" [herokuFolderName] ""

fileNameForEnv :: String -> String
fileNameForEnv name = "zephyr-" ++ lowerString name

pullRepo name = do
  readProcessWithExitCode "git" ["-C", herokuFolderName ++ (fileNameForEnv name), "pull", "-s", "ours"] ""
  processEnvironment name

cloneRepo name = do 
  let url = gitUrlFor name
  putStrLn $ "Creating a local copy of: " ++ name ++ " in " ++ herokuFolderName ++ fileNameForEnv name
  readProcessWithExitCode "git" ["-C", herokuFolderName, "clone", url] ""
  processEnvironment name

hasLocalCopyOfRepo :: String -> IO Bool
hasLocalCopyOfRepo name = do 
  (_, localDirContents, _) <- readProcessWithExitCode "ls" [herokuFolderName] ""
  return $ T.isInfixOf (T.pack $ fileNameForEnv name) (T.pack localDirContents)

checkRepo name = do
  isTrue <- hasLocalCopyOfRepo name 
  if isTrue then pullRepo name
  else cloneRepo name


lastCommitterName :: String -> IO String
lastCommitterName environment = do 
 (_, name,_) <- readProcessWithExitCode "git" ["-C", herokuFolderName ++ fileNameForEnv environment, "show", "--format=format:\"%an\"", "-s"] ""
 return name

pivotalStoriesAccepted :: [String] -> IO Bool
pivotalStoriesAccepted storyIds = (liftM allStoriesCompleted) $ mapM getStoryState storyIds where
  getStoryState :: StoryId -> IO T.Text
  getStoryState storyId = do 
    apiToken <- liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN"
    let options = defaults & header "X-TrackerToken" .~ [apiToken] 
    res <- tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId) 
    case res of 
      Right response -> return $ response ^. responseBody . key "current_state" . _String
      Left (StatusCodeException status headers _) -> do
        case NHT.statusCode status of
          403 -> return "invalid_story_id"
          404 -> return "invalid_story_id"
          _   -> do
            putStrLn $ "Could not process request for story: " ++ storyId ++ " defaulting to not accepted"
            return "unaccepted"
    where 
      tryRequest :: IO a ->  IO (Either HttpException a)
      tryRequest = E.try

  allStoriesCompleted :: [T.Text] -> Bool
  allStoriesCompleted = all acceptedOrInvalidId where
    acceptedOrInvalidId :: T.Text -> Bool
    acceptedOrInvalidId status = status == "accepted" || status == "invalid_story_id"


parseStoryIds :: String -> IO [StoryId]
parseStoryIds env = liftM storyIdForCommit commitMessages  where
  storyIdForCommit :: [String] -> [StoryId]
  storyIdForCommit commitMessages = DL.nub . concat $ MB.mapMaybe parseStoryId commitMessages
    where
      parseStoryId :: String -> Maybe [StoryId]
      parseStoryId = TR.matchRegex (TR.mkRegex "#([0-9]*)")
  commitMessages :: IO [String]
  commitMessages = do 
    messages <- mapM commitMessage [0..12]
    return $ messages
    where
      commitMessage :: Int -> IO String
      commitMessage commitNum = do 
        (_, name,_) <- readProcessWithExitCode "git" ["-C", herokuFolderName ++ fileNameForEnv env, "show", "HEAD~" ++ show commitNum, "--format=format:\"%s\"", "-s"] ""
        return name


analyzeCommits :: EnvironmentName -> IO Environment
analyzeCommits environment = do 
  name <- lastCommitterName environment
  isFree <- (pivotalStoriesAccepted <=< parseStoryIds) environment
  return $ Environment { lastCommitter = name, environmentName = environment, storiesAccepted = isFree }

environmentNames = [ "Alpha", "Echo", "Foxtrot", "Juliet", "Romeo", "Tango", "Whiskey" ] 

processEnvironment envName = do
  analyzeCommits envName >>= putStrLn . show

main :: IO ()
main = do 
  makeHerokuFolder
  putStrLn "Checking environments"
  MP.mapM checkRepo environmentNames >> return ()
