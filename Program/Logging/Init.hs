module Program.Logging.Init(withLogging) where
import Program.Types(Interaction, Program)
import Program.Logging.Types
import Program.Logging.Commands(logDebug)
import Control.Monad.Free(Free(..))
import Data.Functor.Coproduct(left, right, Coproduct)

withLogging :: Program a -> Free (Coproduct Log Interaction) a
withLogging = error "adding logging"
