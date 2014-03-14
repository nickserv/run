package main

import (
  "encoding/json"
  "errors"
  "flag"
  "fmt"
  "io/ioutil"
  "log"
  "os"
  "os/exec"
  "path"
  "path/filepath"
  "runtime"
  "strings"
  "syscall"
)

// The current version number of Run. Run versions will be tagged in the git
// repo, so this is merely provided as a convenience.
const version = "0.1.0"

// The collection of supported commands. This is represented as a map of file
// extensions (strings) to commands (strings).
var commands = map[string]string {
  "c": "gcc % && ./a.out",
  "clj": "clj %",
  "coffee": "coffee %",
  "go": "go run %",
  "hs": "runhaskell %",
  "jar": "java -jar %",
  "lua": "lua %",
  "ml": "ocaml %",
  "py": "python %",
  "rb": "ruby %",
  "sh": "sh %",
  "zsh": "zsh %",
}

// callerDir returns the directory of this source code file in Run's
// implementation. Similar to __dir__ in Ruby.
func callerDir() string {
  _, callerFile, _, _ := runtime.Caller(1)
  return path.Dir(callerFile)
}

// getCommands gets a collection of commands from a JSON config file. This is
// represented as a map of file extensions (strings) to commands (strings).
func getCommands(file string) (map[string]string, error) {
  // Load the commands from the data file to a slice of bytes.
  var commands map[string]string
  jsonStream, fileErr := ioutil.ReadFile(file)
  if fileErr != nil {
    return commands, fileErr
  }

  // Parse the byte slice to get a map of type commands.
  jsonErr := json.Unmarshal(jsonStream, &commands)
  return commands, jsonErr
}

// commandForFile returns the command that should be used to run the given file.
// The beginning of the command depends on the extension of the file, while the
// file path portion(s) of the command will automatically be substituted with
// the given file path.
func commandForFile(path string) (string, error) {
  extension := strings.Replace(filepath.Ext(path), ".", "", -1)

  // Fill out the command template.
  if command, success := commands[extension]; success {
    return strings.Replace(command, "%", path, -1), nil
  }
  return "", fmt.Errorf("run %s: could not determine how to run the file because \"%s\" is not a known extension", path, extension)
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

// start takes the command line args given to Run. It a filename is given as the
// first argument, the command to run it is returned. Otherwise, it returns an
// error. This mostly exists for testing purposes so that the args for main
// won't need to be mocked.
func start(args ...string) (string, error) {
  if len(args) <= 1 {
    return "", errors.New("run: no files given to run")
  }
  return commandForFile(args[1])
}

// merge merges the contents of map2 into map1, using map1 as the default
// values.
func merge(map1 map[string]string, map2 map[string]string) {
  for key, value := range map2 {
    map1[key] = value
  }
}

// printCommands prints the given map of commands in the following format.
//
//   extension: command
//   extension: command
func printCommands(commands map[string]string) {
  for extension, command := range commands {
    fmt.Printf("%s: %s\n", extension, command)
  }
}

// main runs start and executes the resulting command if it succeeds. Otherwise,
// it returns an error.
func main() {
  verbosePtr := flag.Bool("verbose", false, "displays information on all commands that are run, whether or not they are successful")
  dryRunPtr := flag.Bool("dry-run", false, "don't actually run the file, just show any error messages and verbose messages from Run")
  listPtr := flag.Bool("list", false, "list all supported extensions and commands and stop")
  flag.Parse()

  if *listPtr {
    fmt.Println("Note that every % will be replaced with the given filename.")
    fmt.Println()
    printCommands(commands)
    os.Exit(0)
  }

  args := append([]string{os.Args[0]}, flag.Args()...)
  command, err := start(args...)
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
