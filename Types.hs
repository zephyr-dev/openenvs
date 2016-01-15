{-# LANGUAGE DeriveFunctor     #-}
module Types where
import Control.Monad.Free(Free(..))

type TrackerApi = Free TrackerApiInteraction

data TrackerApiInteraction next = TrackerApiInteraction next deriving (Functor)

data Config = Config { pivotalApiToken :: String, herokuFolderPath :: String } deriving Show
