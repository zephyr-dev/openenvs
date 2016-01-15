{-# LANGUAGE DeriveFunctor #-}
module Git.Types where

import Control.Monad.Free(Free(..))
type Git = Free GitAction

data GitAction next = GitPull String next | GitClone String String next deriving(Functor)
