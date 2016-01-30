{-# LANGUAGE DeriveFunctor     #-}
module Program.Logging.Types where
import Control.Monad.Free(Free(..))
import Program.Types

data LogF a = Debug a deriving Functor

type Log = Free LogF
