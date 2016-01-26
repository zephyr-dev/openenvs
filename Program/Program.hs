{-# LANGUAGE OverloadedStrings #-}
module Program.Program(runProgram) where
import Program.Types(Program)
import Control.Monad((>=>))
import Program.Commands


runProgram :: Program ()
runProgram = do
  token            <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
  herokuFolderPath <- (++ "/heroku_envs/") <$> getHomeDir'
  createDirectoryIfMissing' False herokuFolderPath
  _ <- mapM (updateEnvironment herokuFolderPath) environments
  _ <- mapM (parseCommitLog herokuFolderPath >=> getPivotalStories token) environments
  return ()

updateEnvironment path (name, repo) = do
    let fullPath = path ++ name
    doesDirectoryExist <- doesDirectoryExist' fullPath
    if doesDirectoryExist then gitPull' fullPath
    else gitClone' path repo

environments :: [(String, String)]
environments = [("openenvs", "https://github.com/zephyr-dev/openenvs.git")]

