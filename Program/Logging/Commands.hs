module Program.Logging.Commands(logDebug) where
import Program.Logging.Types
import Program.Types
import Git.Types(optionsToString, showOptions)
import Control.Monad.Free(liftF, Free(..))

logDebug :: Program a -> Log ()
logDebug (Free (GetEnv s _)                     )   = liftF $ Debug ("Retrieving environment Variable: " ++ s) ()
logDebug (Free (PrintF _ _)                      )  = liftF $ Debug "Executing print statement" ()
logDebug (Free (GetHomeDir _)                   )   = liftF $ Debug "Finding home directory" ()
logDebug (Free (DoesDirectoryExist s _)         )   = liftF $ Debug ("Checking if '"  ++ s ++ "' exists") ()
logDebug (Free (CreateDirIfMissing _ string _))     = liftF $ Debug ("Creating Directory if missing: " ++ string) ()
logDebug (Free (GetStory _ s1 _)                )   = liftF $ Debug ("Fetching story with id: " ++ s1) ()
logDebug (Free (GitPull s _)                  )     = liftF $ Debug ("Pulling Repo: " ++ s) ()
logDebug (Free (GitClone _ s1 _)                  ) = liftF $ Debug ("Cloning Repo: " ++ s1) ()
logDebug (Free (GitShow opts _)                  )  = liftF $ Debug ("Running Command: git " ++ (optionsToString $ showOptions opts)) ()
logDebug (Pure _)                                   = liftF $ Debug "Extracting value from command" ()
