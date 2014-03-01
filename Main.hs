import System.Environment
import System.IO
import System.Process

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

commandForExtension :: String -> Maybe String
commandForExtension extension = lookup extension commands

start :: [String] -> IO ()
start args = case commandForExtension (head args) of
               Just command -> do
                 System.Process.runCommand command
                 return ()
               Nothing -> hPutStrLn stderr "Error: Extension not found"

main = System.Environment.getArgs >>= start
