{-# LANGUAGE OverloadedStrings #-}
--git ls-remote  git@heroku.com:zephyr-romeo.git
--GET /repos/:owner/:repo/commits/:sha
--curl -u zephyr-dev@googlegroups.com https://api.github.com/repos/zephyr-dev/gust/git/commits/77f5947728753cb7afe6f0cfb9ae71a3597bf7e3
--
--
--
import Network.Wreq(getWith, defaults, oauth2Token, auth, responseBody)
import Control.Monad.Reader
import qualified Data.Text as T
import System.Process(readProcessWithExitCode)
import Control.Lens((.~), (&), (^.), (?~))
import Data.Aeson.Lens (key, _String)
import Data.List.Split(splitOn)
import qualified Control.Monad.Parallel as MP
import qualified Data.ByteString.Lazy as BS

type GitUrl = String
type SHA = String

data Environment = Environment { lastSHA :: String, gitUrl :: GitUrl, environmentName :: String, lastCommitter :: String }
opts = defaults & auth ?~ oauth2Token ""
instance Show Environment where
  show (Environment _ _ name lastCommitter) = name ++ ": " ++ lastCommitter


gustEnvironments :: [Environment]
gustEnvironments = map (\(url, name) -> Environment "" url name "") [ ("git@heroku.com:zephyr-romeo.git", "Romeo"), ("git@heroku.com:zephyr-alpha.git", "Alpha"), ("git@heroku.com:zephyr-echo.git", "Echo"), ("git@heroku.com:zephyr-foxtrot.git", "Foxtrot"), ("git@heroku.com:zephyr-tango.git", "Tango"), ("git@heroku.com:zephyr-whiskey.git", "Whiskey") ] 

addSha :: Environment -> IO Environment
addSha (Environment _ url name _) = do 
  sha <- lastDeployedSha url 
  return (Environment sha url name "")

addLastCommitter :: Environment -> IO Environment
addLastCommitter (Environment sha url name _) = do 
  res <- getWith opts $ "https://api.github.com/repos/zephyr-dev/gust/git/commits/" ++ sha
  let commiterName = T.unpack $ res ^. responseBody . key "committer" . key "name" . _String
  return (Environment sha url name commiterName)



lastDeploys :: IO [Environment]
lastDeploys = do 
  environments <- MP.mapM addSha gustEnvironments
  MP.mapM addLastCommitter environments

lastDeployedSha :: GitUrl -> IO SHA
lastDeployedSha gitUrl  = do 
  (_, result, _) <- readProcessWithExitCode "git" ["ls-remote", gitUrl] ""
  let sha = head $ splitOn "\t" result
  return sha


printHerokuEnvironments :: ReaderT String IO ()
printHerokuEnvironments = do 
  deploys <- liftIO lastDeploys 
  liftIO $ mapM_ print deploys

main :: IO ()
main = do 
   runReaderT printHerokuEnvironments "Some Content"
