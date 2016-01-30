module Program.Logging.Interpreter(interpretIO) where
import Program.Logging.Types
import Control.Monad.Free(Free(..))
import Program.Types(Interaction)
import Data.Functor.Coproduct(Coproduct)
import Control.Monad.Trans.Either(EitherT)

type EIO = EitherT String IO
interpretIO :: Free (Coproduct Log Interaction) a -> EIO a
interpretIO = error "logging interpreter"
