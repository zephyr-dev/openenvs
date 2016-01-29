{-# LANGUAGE OverloadedStrings #-}
module Program.Program(program) where
import Program.Environments
import Control.Applicative((<$>))
import PivotalTracker.Story(storyIdsFromCommits)
import Program.Types(Program, Environment(Environment))
import Control.Monad((>=>), forM_)
import Git.Types(GitOption(..), FormatOption(..))
import Program.Commands


program :: Program ()
program = do
  token            <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
  herokuFolderPath <- (++ "/heroku_envs/") <$> getHomeDir'
  createDirectoryIfMissing' False herokuFolderPath
  _ <- mapM (updateEnvironment herokuFolderPath) environments
  forM_ environments $ \env-> do
    let fullPath = pathTo herokuFolderPath env
    recentStoryIds <- storyIdsFromCommits <$> mapM (\int -> gitShow' [Path fullPath, Head int, Format Subject, NoPatch] ) [0..12]
    stories <- getPivotalStories token recentStoryIds
    lastCommitterName <- read <$> gitShow' [Path fullPath, Format AuthorName, NoPatch]
    let environment =  Environment (humanName env) lastCommitterName stories
    print' environment

updateEnvironment :: String -> HerokuEnvironment -> Program ()
updateEnvironment path env = do
    let fullPath = pathTo path env
    doesDirectoryExist <- doesDirectoryExist' fullPath
    if doesDirectoryExist then gitPull' fullPath
    else gitClone' path (gitRepoFor env)

environments :: [HerokuEnvironment]
environments = [Alpha .. Whiskey]
