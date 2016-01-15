module System.Interpreters.IO(interpretIO) where
import System.Directory(doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import Control.Monad.Free(Free(..), liftF)
import System.Environment(getEnv)
import System.Types

interpretIO :: System a -> IO a
interpretIO (Free (GetEnv s fn)) = getEnv s >>= interpretIO . fn
interpretIO (Free (PrintF s n)) = print s >> interpretIO n
interpretIO (Free (GetHomeDir f)) = getHomeDirectory >>= interpretIO . f
interpretIO (Free (CreateDirIfMissing b dir n)) = createDirectoryIfMissing b dir >> interpretIO n
interpretIO (Free (DoesFileExist file f)) = doesFileExist file >>= interpretIO . f
interpretIO (Pure a)          = return a
