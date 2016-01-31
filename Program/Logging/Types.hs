{-# LANGUAGE DeriveFunctor     #-}
module Program.Logging.Types where
import Control.Monad.Free(Free(..))
import Program.Types

data LogF a = Debug String a deriving (Functor, Show)

type Log = Free LogF
