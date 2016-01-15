module System.Commands where
import Control.Monad.Free(liftF, Free(..))
import System.Types

print' :: String -> System ()
print' s = liftF $ PrintF s ()

getEnv' :: String -> System String
getEnv' key = liftF $ GetEnv key id

createDirectoryIfMissing' :: Bool -> String -> System ()
createDirectoryIfMissing' bool dir = liftF $ CreateDirIfMissing bool dir ()

doesFileExist' :: String -> System Bool
doesFileExist' fileName = liftF $ DoesFileExist fileName id

getHomeDir' :: System String
getHomeDir' = liftF $ GetHomeDir id
