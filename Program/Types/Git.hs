module Program.Types.Git where
import Data.List

data FormatOption = Subject | AuthorName deriving (Show, Eq, Ord)

data GitOption = Head Int | Format FormatOption | NoPatch | Path String deriving (Show, Eq, Ord)

optionsToCliArguments :: [GitOption] -> [String]
optionsToCliArguments options = foldr (\opt acc -> optionToArg opt ++ acc ) []  $ sort options

optionToArg :: GitOption -> [String]
optionToArg (Head i) = ["HEAD~" ++  show i]
optionToArg (Format Subject) = ["--format=format:\"%s\""]
optionToArg (Format AuthorName) = ["--format=format:\"%an\""]
optionToArg (Path path) = ["-C", path]
optionToArg NoPatch = ["-s"]

