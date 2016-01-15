{-# LANGUAGE DeriveFunctor     #-}
module System.Types where
import Control.Monad.Free(Free(..))
type System = Free Interaction

data Interaction next =
  GetEnv String (String -> next) |
  PrintF String next | 
  GetHomeDir (String -> next) |
  DoesFileExist String (Bool -> next) |
  CreateDirIfMissing Bool String next  deriving (Functor)
