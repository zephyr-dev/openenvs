{-# LANGUAGE OverloadedStrings #-}
module Program.Program(runProgram) where
import Control.Applicative((<$>))
import PivotalTracker.Story(storyIdsFromCommits)
import Program.Types(Program)
import Control.Monad((>=>), forM_)
import Program.Commands


runProgram :: Program ()
runProgram = do
  token            <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
  herokuFolderPath <- (++ "/heroku_envs/") <$> getHomeDir'
  createDirectoryIfMissing' False herokuFolderPath
  _ <- mapM (updateEnvironment herokuFolderPath) environments
  forM_ environments $ \(name, _) -> do
    recentStoryIds <- storyIdsFromCommits <$> mapM (\i -> gitShow' (herokuFolderPath ++ name) i) [0..12]
    stories <- getPivotalStories token recentStoryIds
    return ()

updateEnvironment path (name, repo) = do
    let fullPath = path ++ name
    doesDirectoryExist <- doesDirectoryExist' fullPath
    if doesDirectoryExist then gitPull' fullPath
    else gitClone' path repo

environments :: [(String, String)]
environments = [("openenvs", "https://github.com/zephyr-dev/openenvs.git")]

