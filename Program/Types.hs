{-# LANGUAGE DeriveFunctor     #-}
module Program.Types where
import Control.Monad.Free(Free(..))
type Program = Free Interaction

data Interaction next =
  GetEnv String (String -> next) |
  PrintF String next |
  GetHomeDir (String -> next) |
  DoesFileExist String (Bool -> next) |
  CreateDirIfMissing Bool String next |
  GitPull String next |
  GitClone String String next   deriving (Functor)

