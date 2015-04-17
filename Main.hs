{-# LANGUAGE OverloadedStrings #-}
--git ls-remote  git@heroku.com:zephyr-romeo.git
--GET /repos/:owner/:repo/commits/:sha
--curl -u zephyr-dev@googlegroups.com https://api.github.com/repos/zephyr-dev/gust/git/commits/77f5947728753cb7afe6f0cfb9ae71a3597bf7e3
--
--
--
import Data.Char(toLower)
import System.Environment
import Network.Wreq(getWith, defaults, oauth2Token, auth, responseBody)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Control.Monad.Reader
import qualified Data.Text as T
import System.Process(readProcessWithExitCode)
import Control.Lens((.~), (&), (^.), (?~))
import Data.Aeson.Lens (key, _String)
import Data.List.Split(splitOn)
import qualified Control.Monad.Parallel as MP
import qualified Data.ByteString.Lazy as BL

type GitUrl = String
type SHA = String

data Environment = Environment { lastSHA :: String, gitUrl :: GitUrl, environmentName :: String, lastCommitter :: String }

opts = do 
  token <- ask 
  return $ defaults & auth ?~ oauth2Token token

instance Show Environment where
  show (Environment _ _ name lastCommitter) = name ++ ": " ++ lastCommitter

lowerString :: [Char] -> [Char]
lowerString = map toLower


gitUrlFor :: String -> GitUrl
gitUrlFor envName = "git@heroku.com:zephyr-" ++ lowerString envName ++ ".git"

gustEnvironments :: [Environment]
gustEnvironments = Prelude.map (\name -> Environment "" (gitUrlFor name) name "") [ "Romeo", "Alpha", "Echo", "Foxtrot", "Tango", "Whiskey" ] 

addSha :: Environment -> IO Environment
addSha (Environment _ url name _) = do 
  sha <- lastDeployedSha url 
  return (Environment sha url name "")

addLastCommitter :: Environment -> ReaderT BS.ByteString IO Environment
addLastCommitter (Environment sha url name _) = do 
  options <- opts
  res <- liftIO $ getWith options $ "https://api.github.com/repos/zephyr-dev/gust/git/commits/" ++ sha
  let commiterName = T.unpack $ res ^. responseBody . key "committer" . key "name" . _String
  return (Environment sha url name commiterName)



lastDeploys :: ReaderT BS.ByteString IO [Environment]
lastDeploys = do 
  environments <- liftIO $ MP.mapM addSha gustEnvironments
  MP.mapM addLastCommitter environments

lastDeployedSha :: GitUrl -> IO SHA
lastDeployedSha gitUrl  = do 
  (_, result, _) <- readProcessWithExitCode "git" ["ls-remote", gitUrl] ""
  let sha = Prelude.head $ splitOn "\t" result
  return sha


printHerokuEnvironments :: ReaderT BS.ByteString IO ()
printHerokuEnvironments = do 
  deploys <- lastDeploys 
  liftIO $ mapM_ print deploys

main :: IO ()
main = do 
   token <- getEnv "GITHUB_TOKEN"
   runReaderT printHerokuEnvironments $ BSC.pack token
