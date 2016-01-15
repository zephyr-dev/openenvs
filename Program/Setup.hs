module Program.Setup where
import Control.Applicative((<$>))
import Types(TrackerApi, Config(..))
import Program.Commands

setup :: Setup Config
setup = do
  herokuPath <- (++ "/heroku_envs/" ) <$> getHomeDir'
  trackerApiToken  <- (getEnv' "PIVOTAL_TRACKER_API_TOKEN")
  createDirectoryIfMissing' False herokuPath
  return $ Config trackerApiToken herokuPath
