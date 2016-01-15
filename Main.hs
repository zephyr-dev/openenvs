{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified System.Interpreters.IO as SI
import qualified Heroku.Interpreters.IO as HI
import System.Commands(getEnv', createDirectoryIfMissing', getHomeDir', print')


main :: IO ()
main = do
  config <- SI.interpretIO $ do
    token <- getEnv' "PIVOTAL_TRACKER_API_TOKEN"
    return 
  
