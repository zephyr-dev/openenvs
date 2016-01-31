module Program.Logging.Init(withLogging) where
import Program.Types(Interaction(..), Program)
import Program.Logging.Types
import Program.Logging.Commands(logDebug)
import Control.Monad.Free(Free(..), liftF)
import Data.Functor.Coproduct(left, right, Coproduct)

-- :t (*>) :: f a -> f b -> f b
-- Free (Coproduct LogF Interaction) a -> Free (Coproduct LogF Interaction) b -> Free (Coproduct LogF Interaction) b 
-- Coproduct Left Debug "GetEnv" *> Coproduct Right GetEnv "GetENv" -> Coproduct Right GetEnv "GetENv"
withLogging :: Program a -> Free (Coproduct LogF Interaction) a
withLogging program = toLeft (logDebug program) *> toRight program 

toLeft :: Log a -> Free (Coproduct LogF Interaction) a
toLeft (Free f) = liftF (left f) >>= toLeft
toLeft (Pure a) = Pure a

toRight :: Program a -> Free (Coproduct LogF Interaction) a
toRight (Free f) =  liftF (right f) >>= withLogging
toRight (Pure a) = Pure a
