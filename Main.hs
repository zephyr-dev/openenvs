module Main where
import Control.Monad.Trans.Either(runEitherT)
import System.Environment(getArgs)
import Data.List(intercalate)
import qualified Program.Logging.Interpreter as LoggingInterpreter
import qualified Program.Interpreter as Interpreter
import Program.Logging.Init(withLogging)
import Program.Program(program)

main :: IO ()
main = do
  putStrLn "Checking Environments"
  args <- getArgs
  case args of
    ["--verbose"] -> runEitherT (LoggingInterpreter.interpretIO (withLogging program)) >>= handleResult
    [] -> runEitherT (Interpreter.interpretIO program) >>= handleResult
    _  -> putStrLn $  "Unrecognized option(s): " ++ (intercalate ", "  args) ++ "\n" ++ "Options are: [--verbose]"

handleResult :: Either String () -> IO ()
handleResult result = case result of
                        Left a -> putStrLn $ "Error: " ++ a
                        Right _ -> return ()
