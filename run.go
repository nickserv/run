package main

import (
  "encoding/json"
  "flag"
  "fmt"
  "io/ioutil"
  "log"
  "os"
  "os/exec"
  "path"
  "runtime"
  "strings"
  "syscall"
)

// The current version number of Run. Run versions will be tagged in the git
// repo, so this is merely provided as a convenience.
const version = "0.1.0"

// A collection of information about different languages. This is represented as
// a map from language names (strings) to language structs.
type languageCollection map[string]language

// A language struct represents the information that Run knows about any
// executable language. Extension represents the main file extension for files
// written in this language. Command represents the command template used to run
// files of this type, with a "%" representing the appropriate filename.
type language struct {
  Extension string
  Command string
}

// The default languageCollection included with Run.
var defaultLanguages = languageCollection {
  "c": language{"c", "gcc % && ./a.out"},
  "clojure": language{"clj", "clj %"},
  "coffeescript": language{"coffee", "coffee %"},
  "go": language{"go", "go run %"},
  "haskell": language{"hs", "runhaskell %"},
  "java jar": language{"jar", "java -jar %"},
  "lua": language{"lua", "lua %"},
  "ocaml": language{"ml", "ocaml %"},
  "python": language{"py", "python %"},
  "ruby": language{"rb", "ruby %"},
  "sh": language{"sh", "sh %"},
  "zsh": language{"zsh", "zsh %"},
}

// callerDir returns the directory of this source code file in Run's
// implementation. Similar to __dir__ in Ruby.
func callerDir() string {
  _, callerFile, _, _ := runtime.Caller(1)
  return path.Dir(callerFile)
}

// getLanguages gets a languageCollection from a JSON config file. This is
// represented as a map of file extensions (strings) to commands (strings).
func getLanguages(file string) (languageCollection, error) {
  // Load the languages from the data file to a slice of bytes.
  var languages languageCollection
  jsonStream, fileErr := ioutil.ReadFile(file)
  if fileErr != nil {
    return languages, fileErr
  }

  // Parse the byte slice to get a value of type languageCollection.
  jsonErr := json.Unmarshal(jsonStream, &languages)
  return languages, jsonErr
}

// runCommand finds the appropriate command to run a file and executes it,
// replacing the current process. If the command fails, an error will be
// returned.
func runCommand(command string) error {
  // Separate the command into arguments for syscall.Exec.
  args := strings.Split(command, " ")

  // Get the path to the binary.
  binary, err := exec.LookPath(args[0])
  if err != nil {
    return err
  }

  // Execute the command, replacing the current process with it.
  return syscall.Exec(binary, args, os.Environ())
}

// main runs start and executes the resulting command if it succeeds. Otherwise,
// it returns an error.
func main() {
  languagePtr := flag.String("language", "", "force a language to use instead of the given file's predicted language (if there is one)")
  verbosePtr := flag.Bool("verbose", false, "displays information on all commands that are run, whether or not they are successful")
  dryRunPtr := flag.Bool("dry-run", false, "don't actually run the file, just show any error messages and verbose messages from Run")
  listPtr := flag.Bool("list", false, "list all known language information (extensions and commands) and stop")
  flag.Parse()

  if *listPtr {
    fmt.Println("Note that every % will be replaced with the given filename.")
    fmt.Println()
    fmt.Println(defaultLanguages.string())
    os.Exit(0)
  }

  if len(flag.Args()) == 0 {
    log.Fatal("run: no files given to run")
  }

  file := flag.Args()[0]
  var command string
  var err error
  if *languagePtr == "" {
    command, err = defaultLanguages.commandForFile(file)
  } else {
    command, err = defaultLanguages.commandForFileAndLanguage(file, *languagePtr)
  }
  if err != nil {
    log.Fatal(err)
  }

  if *verbosePtr {
    fmt.Println(command)
  }
  if !*dryRunPtr {
    runCommand(command)
  }
}
