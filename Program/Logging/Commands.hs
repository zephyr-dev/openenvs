module Program.Logging.Commands(logDebug) where
import Program.Logging.Types
import Program.Types
import Control.Monad.Free(liftF, Free(..))

logDebug :: Program a -> Log String
logDebug (Free (GetEnv s fn)                     ) = liftF $ Debug "Debggin"
logDebug (Free (PrintF s n)                      ) = liftF $ Debug "Debggin"
logDebug (Free (GetHomeDir fn)                   ) = liftF $ Debug "Debggin"
logDebug (Free (DoesDirectoryExist s fn)         ) = liftF $ Debug "Debggin "
logDebug (Free (Const a fn)                      ) = liftF $ Debug "Debggin "
logDebug (Free (CreateDirIfMissing bool string n)) = liftF $ Debug "Debggin "
logDebug (Free (GetStory s s1 fn)                ) = liftF $ Debug "Debggin "
logDebug (Free (GitPull s next)                  ) = liftF $ Debug "Debggin "
logDebug (Free (GitShow xs fn)                   ) = liftF $ Debug "Debggin "
logDebug (Free (GitClone s s1 next)              ) = liftF $ Debug "Debggin "
