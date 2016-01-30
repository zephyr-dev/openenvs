module Main where
import Control.Monad.Trans.Either(runEitherT)
import Program.Logging.Interpreter(interpretIO)
import Program.Logging.Init(withLogging)
import Program.Program(program)

main :: IO ()
main = do
  putStrLn "Checking Environments"
  result <- runEitherT $ interpretIO $ withLogging program
  case result of
    Left a -> putStrLn $ "Error: " ++ a
    Right a -> return ()
