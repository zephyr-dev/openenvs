module Program.Logging.Init(withLogging) where
import Program.Types(Interaction(..), Program)
import Program.Logging.Types
import Program.Logging.Commands(logDebug)
import Control.Applicative((*>), (<$>))
import Control.Monad.Free(Free(..), liftF)
import Data.Functor.Coproduct(left, right, Coproduct)

withLogging :: Program a -> Free (Coproduct LogF Interaction) a
withLogging program = (toLeft (logDebug program)) *> (toRight program)


toLeft :: Log a -> Free (Coproduct LogF Interaction) a
toLeft (Pure a) = liftF $ left (Id a)
toLeft (Free f) = liftF (left f) >>= toLeft 
toRight :: Free Interaction a -> Free (Coproduct LogF Interaction) a
toRight (Pure a) = liftF $ right (Const a id)
toRight (Free f) =  liftF (right f) >>= toRight
