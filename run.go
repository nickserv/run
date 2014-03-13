package main

import (
  "encoding/json"
  "errors"
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
const version = "0.0.1"

// callerDir returns the directory of this source code file in Run's
// implementation. Similar to __dir__ in Ruby.
func callerDir() string {
  _, callerFile, _, _ := runtime.Caller(1)
  return path.Dir(callerFile)
}

// getCommands gets the collection of supported commands. This is represented as
// a map of file extensions (strings) to commands (strings), loaded from the
// data file (commands.json).
func getCommands() (map[string]string, error) {
  // Load the commands from the data file to a slice of bytes.
  var commands map[string]string
  jsonStream, fileErr := ioutil.ReadFile(path.Join(callerDir(), "commands.json"))
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
  commands, err := getCommands()
  if err != nil {
    return "", err
  }

  extension := strings.Replace(filepath.Ext(path), ".", "", -1)

  // Fill out the command template.
  if command, success := commands[extension]; success {
    return strings.Replace(command, "%", path, -1), nil
  }
  return "", errors.New("run could not determine how to run this file because it does not have a known extension")
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
    return "", errors.New("no files given")
  }
  return commandForFile(args[1])
}

// main runs start and executes the resulting command if it succeeds. Otherwise,
// it returns an error.
func main() {
  command, err := start(os.Args...)
  if err != nil {
    log.Fatal(err)
  }

  fmt.Println(command)
  runCommand(command)
}
