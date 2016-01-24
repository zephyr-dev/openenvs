{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Trans.Either(runEitherT)
import Program.Interpreter(interpretIO)
import Program.Commands


environments :: [(String, String)]
environments = [("openenvs", "https://github.com/zephyr-dev/openenvs.git")]

main :: IO ()
main = do
  result <- runEitherT $ interpretIO $ do
    token            <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
    herokuFolderPath <- (++ "/heroku_envs/") <$> getHomeDir'
    createDirectoryIfMissing' False herokuFolderPath
    mapM (updateEnvironment herokuFolderPath) environments
  case result of
    Right a -> putStrLn $ "Success!"
    Left a -> putStrLn $ "Error: " ++ a


updateEnvironment path (name, repo) = do
    let fullPath = path ++ name
    doesFileExist <- doesFileExist' fullPath
    if doesFileExist then gitPull' fullPath
    else gitClone' path repo
