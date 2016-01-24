{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans.Either(runEitherT)
import Program.Interpreter(interpretIO)
import Program.Commands


main :: IO ()
main = do
  runEitherT $ interpretIO $ do
    token            <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
    herokuFolderPath <- (++ "/heroku_envs/") <$> getHomeDir'
    createDirectoryIfMissing' False herokuFolderPath
    doesFileExist <- doesFileExist' "openenvs"
    if doesFileExist then gitClone' herokuFolderPath "https://github.com/zephyr-dev/openenvs.git"
    else gitPull' "openenvs"
  return ()
