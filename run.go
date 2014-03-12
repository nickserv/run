package main

import (
  "bytes"
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
)

const version = "0.0.1"

type commands map[string]string

func callerDir() string {
  _, callerFile, _, _ := runtime.Caller(1)
  return path.Dir(callerFile)
}

func getCommands() (commands, error) {
  var commands commands
  jsonStream, err := ioutil.ReadFile(path.Join(callerDir(), "commands.json"))
  if err != nil {
    return commands, err
  }

  decoder := json.NewDecoder(bytes.NewReader(jsonStream))
  if err := decoder.Decode(&commands); err != nil {
    return commands, err
  }

  return commands, nil
}

func commandForFile(path string) (string, error) {
  commands, err := getCommands()
  if err != nil {
    return "", err
  }

  extension := strings.Replace(filepath.Ext(path), ".", "", -1)

  if command := commands[extension]; command != "" {
    return strings.Replace(command, "%", path, -1), nil
  }
  return "", errors.New("run could not determine how to run this file because it does not have a known extension")
}

func runCommand(command string) {
  sections := strings.Split(command, " ")
  name := sections[0]
  args := sections[1:]

  cmd := exec.Command(name, args...)
  var out bytes.Buffer
  cmd.Stdout = &out
  if err := cmd.Run(); err == nil {
    fmt.Println(out.String())
  } else {
    log.Fatal(err)
  }
}

func start(args []string) (string, error) {
  if len(args) <= 1 {
    return "", errors.New("no files given")
  }
  return commandForFile(args[1])
}

func main() {
  if command, err := start(os.Args); err == nil {
    fmt.Println(command)
    runCommand(command)
  } else {
    log.Fatal(err)
  }
}
