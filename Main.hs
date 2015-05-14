{-# LANGUAGE OverloadedStrings #-}
import Data.Char(toLower)
import Control.Applicative((<$>))
import Data.Time.Format     (parseTime, formatTime)
import Data.Time.Clock      (UTCTime)
import System.Console.ANSI
import System.Locale        (defaultTimeLocale)
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

data Environment = Environment { environmentName :: EnvironmentName, lastCommitter :: String, storiesAccepted :: Bool, recentStories :: [PivotalStory] }

storyStatuses :: [PivotalStory] -> String
storyStatuses xs = let dateString = if (length pendingAcceptance > 0)  then "as of " ++ mostRecentSubmittedStoryDate else "" in
                    " " ++ show (length pendingAcceptance) ++ " stories pending acceptance "  ++ dateString where
  mostRecentSubmittedStoryDate = MB.maybe "" id $ formatTime defaultTimeLocale "%D" <$> lastSubmittedDate
  lastSubmittedDate = MB.listToMaybe . reverse . DL.sort  . MB.catMaybes $ map storyUpdatedAt pendingAcceptance
  pendingAcceptance = filter (not . storyAccepted) xs

instance Show Environment where
 show (Environment name lastCommitter True stories) = setSGRCode [SetColor Foreground Dull Green] ++ name ++ ": " ++ read lastCommitter ++ storyStatuses stories ++ "\x1b[0m" 
 show (Environment name lastCommitter False stories) = setSGRCode [SetColor Foreground Dull Red] ++  name ++ ": " ++ read lastCommitter ++ storyStatuses stories ++ "\x1b[0m"


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


data PivotalStory = PivotalStory { pivotalStoryStatus :: T.Text, storyUpdatedAt :: Maybe UTCTime } deriving Show

textToDate  :: String -> Maybe UTCTime
textToDate  = parseTime defaultTimeLocale "%FT%X%QZ"

pivotalStories :: [StoryId] -> IO [PivotalStory]
pivotalStories storyIds = mapM getStory storyIds where
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
          403 -> return $ PivotalStory "invalid_story_id" Nothing
          404 -> return $ PivotalStory "invalid_story_id" Nothing
          _   -> do
            putStrLn $ "Could not process request for story: " ++ storyId ++ " defaulting to not accepted"
            return $ PivotalStory "not_accepted" Nothing
    where 
      tryRequest :: IO a ->  IO (Either HttpException a)
      tryRequest = E.try

storyAccepted :: PivotalStory -> Bool
storyAccepted story = pivotalStoryStatus story == "accepted" || pivotalStoryStatus story == "invalid_story_id"

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
  stories <- (pivotalStories <=< parseStoryIds) environment
  let isFree = all storyAccepted stories
  return $ Environment { lastCommitter = name, environmentName = environment, storiesAccepted = isFree, recentStories = stories }

environmentNames = [ "Alpha", "Echo", "Foxtrot", "Juliet", "Romeo", "Tango", "Whiskey" ] 

processEnvironment envName = do
  analyzeCommits envName >>= putStrLn . show

main :: IO ()
main = do 
  makeHerokuFolder
  putStrLn "Checking environments"
  MP.mapM checkRepo environmentNames >> return ()
