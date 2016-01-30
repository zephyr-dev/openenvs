module Program.Logging.Types where
import Control.Monad.Free(Free(..))
import Program.Types

data LogF a = Debug a

type Log = Free LogF
