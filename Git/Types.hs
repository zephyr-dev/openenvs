module Git.Types where
import Data.List

data FormatOption = Subject | AuthorName deriving (Show, Eq, Ord)

data GitOption = Path String | Show | Head Int | Format FormatOption | NoPatch deriving (Show, Eq, Ord)

optionsToString :: [GitOption] -> String
optionsToString = intercalate " " . optionsToCliArguments

optionsToCliArguments :: [GitOption] -> [String]
optionsToCliArguments options = foldr (\opt acc -> optionToArg opt ++ acc ) []  $ sort options

showOptions :: [GitOption] -> [GitOption]
showOptions opts = Show:opts

optionToArg :: GitOption -> [String]
optionToArg (Head i) = ["HEAD~" ++  show i]
optionToArg (Format Subject) = ["--format=format:\"%s\""]
optionToArg (Format AuthorName) = ["--format=format:\"%an\""]
optionToArg Show = ["show"]
optionToArg (Path path) = ["-C", path]
optionToArg NoPatch = ["-s"]
