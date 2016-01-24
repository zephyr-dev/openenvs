{-# LANGUAGE ExistentialQuantification #-}
module Git.Types where

import Control.Monad.Free(Free(..))
type Git = Free GitAction

data GitAction next =
  GitPull String next
  | forall a b. LiftGit a (b -> next)
  | GitClone String String next


instance Functor GitAction where
  fmap f (GitPull s n) = GitPull s (f n)
  fmap f (LiftGit a fn) = (LiftGit a $ f `fmap` fn)
  fmap f (GitClone s s1 n) = GitClone s s1 (f n)
