module Program.Environments where
import Data.Char(toLower)

data HerokuEnvironment = Alpha | Bravo | Delta | Echo | Foxtrot | Juliet | Romeo | Tango | Whiskey deriving (Show, Enum)

pathTo :: String -> HerokuEnvironment -> String
pathTo basePath env = basePath ++ (envName env)

gitRepoFor :: HerokuEnvironment -> String
gitRepoFor env =  "https://git.heroku.com/" ++ (envName env) ++ ".git"

envName :: HerokuEnvironment -> String
envName = (map toLower) . ("zephyr-" ++) . show

humanName :: HerokuEnvironment -> String
humanName = show
