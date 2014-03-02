-- | The `Main` module for the `run` executable.
module Main where

import System.Environment
import System.IO
import System.Process

-- | Returns pairs of extensions and their appropriate commands, represented by
-- `Strings` with a `%` character to be replaced by a filename.
commands :: [(String, String)]
commands = [ ("c", "gcc % && ./a.out"),
             ("clj", "clj %"),
             ("coffee", "coffee %"),
             ("go", "go run %"),
             ("hs", "runhaskell %"),
             ("jar", "java -jar %"),
             ("lua", "lua %"),
             ("ml", "ocaml %"),
             ("py", "python %"),
             ("rb", "ruby %"),
             ("sh", "sh %"),
             ("zsh", "zsh %") ]

-- | Returns the command for running files of a given extension, or `Nothing`
-- if no commands are found.
commandForExtension :: String -> Maybe String
commandForExtension extension = lookup extension commands

-- | Runs a file with the given command line arguments. For now, this must be a
-- `List` of `Strings` of length one, containing a `String` of the file path.
start :: [String] -> IO ()
start args = case commandForExtension (head args) of
               Just command -> do
                 System.Process.runCommand command
                 return ()
               Nothing -> hPutStrLn stderr "Error: Extension not found"

-- | Runs a file with the command line arguments given at the command line.
main = System.Environment.getArgs >>= start
