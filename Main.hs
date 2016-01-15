{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Git.Interpreters.IO as Git
import qualified Git.Commands as Git
import qualified System.Interpreters.IO as SI
import Control.Monad.Trans.Either(runEitherT)
import System.Commands(getEnv', createDirectoryIfMissing', getHomeDir', print')
import Types(Config(..))


main :: IO ()
main = do
  config <- SI.interpretIO $ do
    token            <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
    herokuFolderPath <- (++ "/heroku_envs/") <$> getHomeDir'
    createDirectoryIfMissing' False herokuFolderPath
    return $ Config token herokuFolderPath
  runEitherT $ Git.interpret $ do
    Git.gitClone' (herokuFolderPath config) "https://github.com/zephyr-dev/openenvs.git"
  return ()
