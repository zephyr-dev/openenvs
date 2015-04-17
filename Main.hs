{-# LANGUAGE OverloadedStrings #-}
--git ls-remote  git@heroku.com:zephyr-romeo.git
--GET /repos/:owner/:repo/commits/:sha


--curl -u zephyr-dev@googlegroups.com https://api.github.com/repos/zephyr-dev/gust/git/commits/77f5947728753cb7afe6f0cfb9ae71a3597bf7e3
--
--
--
import Network.Wreq(getWith, defaults, basicAuth, auth, responseBody)
import System.Process(readProcessWithExitCode)
import Control.Lens((.~), (&), (^.), (?~))
import Data.List.Split(splitOn)
import qualified Data.ByteString.Lazy as BS

type GitUrl = String
type SHA = String

data Environment = Environment { lastSHA :: String, gitUrl :: GitUrl, environmentName :: String }
opts = defaults & auth ?~ basicAuth "zephyr-dev@googlegroups.com" ""

instance Show Environment where
  show (Environment _ url name) = name ++ ": " ++ url


gustEnvironments :: [Environment]
gustEnvironments = map (\(url, name) -> Environment "" url name) [ ("git@heroku.com:zephyr-romeo.git", "Romeo") ] 

addSha :: Environment -> IO Environment
addSha (Environment _ url name) = do 
  sha <- lastDeployedSha url 
  return (Environment sha url name)

lastDeploys :: IO [Environment]
lastDeploys = mapM addSha gustEnvironments

lastDeployedSha :: GitUrl -> IO SHA
lastDeployedSha gitUrl  = do 
  (_, result, _) <- readProcessWithExitCode "git" ["ls-remote", gitUrl] ""
  let sha = head $ splitOn "\t" result
  return sha

main :: IO ()
main = do 
  res <- getWith opts "https://api.github.com/repos/zephyr-dev/gust/git/commits/77f5947728753cb7afe6f0cfb9ae71a3597bf7e3"
  BS.putStrLn $ res ^. responseBody
