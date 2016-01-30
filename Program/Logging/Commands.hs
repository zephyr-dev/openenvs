module Program.Logging.Commands(logDebug) where
import Program.Logging.Types
import Program.Types

logDebug :: Interaction a -> Log ()
logDebug = error "log Debug"
