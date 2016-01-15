
module Program where
import qualified System.Interpreters.IO as SI
import qualified Git.Interpreters.IO as GI

data Program a = Program Git a | Program System a

interpretIO :: Program a -> IO a
interpretIO (Program Git a) = undefined


program :: 
