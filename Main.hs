module Main where
import Control.Monad.Trans.Either(runEitherT)
import Program.Interpreter(interpretIO)
import Program.Program(program)

main :: IO ()
main = do
  putStrLn "Checking Environments"
  result <- runEitherT $ interpretIO program
  case result of
    Left a -> putStrLn $ "Error: " ++ a
    Right a -> putStrLn "success"
