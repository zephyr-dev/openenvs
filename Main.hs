import Data.Char(toLower)
import System.Environment
import qualified Data.Text as T
import System.Process(readProcessWithExitCode)
import qualified Control.Monad.Parallel as MP
type GitUrl = String
type SHA = String
lowerString :: [Char] -> [Char]
lowerString = map toLower

data Environment = Environment { environmentName :: String, lastCommitter :: String }

instance Show Environment where
 show (Environment name lastCommitter) = name ++ ": " ++ lastCommitter


gitUrlFor :: String -> GitUrl
gitUrlFor envName = "git@heroku.com:zephyr-" ++ lowerString envName ++ ".git"

herokuFolderName = "/Users/gust/workspace/heroku_envs/"

makeHerokuFolder = readProcessWithExitCode "mkdir" [herokuFolderName] ""

fileNameForEnv :: String -> String
fileNameForEnv name = "zephyr-" ++ lowerString name

pullRepo name = do
  putStrLn $ "Checking: " ++ name
  readProcessWithExitCode "git" ["-C", herokuFolderName ++ (fileNameForEnv name), "pull", "-s", "ours"] ""
  processEnvironment name

cloneRepo name = do 
  let url = gitUrlFor name
  putStrLn $ "Creating a local copy of: " ++ name ++ " in " ++ herokuFolderName ++ fileNameForEnv name
  readProcessWithExitCode "git" ["-C", herokuFolderName, "clone", url] ""
  processEnvironment name

repoExists :: String -> String -> Bool
repoExists name dirContents = T.isInfixOf (T.pack $ fileNameForEnv name) (T.pack dirContents)

createOrUpdateRepo name = do
  (_, dirContents, _) <- readProcessWithExitCode "ls" [herokuFolderName] ""
  if (repoExists name dirContents) then pullRepo name
  else cloneRepo name

analyzeCommits :: String -> IO Environment
analyzeCommits environment = do 
  (_, name, _) <- readProcessWithExitCode "git" ["-C", herokuFolderName ++ fileNameForEnv environment, "show", "--format=format:\"%an\"", "-s"] ""
  return $ Environment environment (read name)

environmentNames = [ "Alpha", "Echo", "Foxtrot", "Juliet", "Romeo", "Tango", "Whiskey" ] 

processEnvironment envName = do
  analyzeCommits envName >>= putStrLn . show

main :: IO ()
main = do 
  makeHerokuFolder
  MP.mapM createOrUpdateRepo environmentNames >> return ()
