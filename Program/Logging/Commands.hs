module Program.Logging.Commands(logDebug) where
import Program.Logging.Types
import Program.Types
import Control.Monad.Free(liftF, Free(..))

logDebug :: Program a -> Log String
logDebug (Free (GetEnv s fn)                     ) = liftF $ Debug $ "Retreiving environment Variable: " ++ s
logDebug (Free (PrintF s n)                      ) = liftF $ Debug "Executing print statement"
logDebug (Free (GetHomeDir fn)                   ) = liftF $ Debug "Finding home directory"
logDebug (Free (DoesDirectoryExist s fn)         ) = liftF $ Debug $ "Checking if '"  ++ s ++ "' exists"
logDebug (Free (CreateDirIfMissing bool string n)) = liftF $ Debug $ "Creating Directory if missing: " ++ string
logDebug (Free (GetStory s s1 fn)                ) = liftF $ Debug $ "Getting story: " ++ s ++ " " ++ s1
logDebug (Free (GitPull s next)                  ) = liftF $ Debug $ "Pulling Repo: " ++ s
logDebug (Free (GitShow xs fn)                   ) = liftF $ Debug "Git show"
logDebug (Free (GitClone s s1 next)              ) = liftF $ Debug $ "Cloning Repo: " ++ s1 ++ " " ++ s1
logDebug _                                         = liftF $ Debug $ "No Debug info defined"
