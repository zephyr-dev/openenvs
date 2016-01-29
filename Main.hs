module Main where
import Control.Monad.Trans.Either(runEitherT)
import Program.Interpreter(interpretIO)
import Program.Program(runProgram)

main :: IO ()
main = do
  putStrLn "Checking Environments"
  result <- runEitherT $ interpretIO runProgram
  case result of
    Left a -> putStrLn $ "Error: " ++ a
    Right a -> putStrLn "success"
