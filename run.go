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
  "path/filepath"
  "strings"
)

const Version = "0.0.1"

type Commands map[string]string

func getCommands() (Commands, error) {
  var commands Commands
  jsonStream, err := ioutil.ReadFile("./commands.json")
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

func start(args []string) (string, error) {
  if len(args) <= 1 {
    return "", errors.New("no files given")
  }
  return commandForFile(args[1])
}

func main() {
  if command, err := start(os.Args); err == nil {
    sections := strings.Split(command, " ")
    name := sections[0]
    args := sections[1:]

    fmt.Println(command)
    cmd := exec.Command(name, args...)
    var out bytes.Buffer
    cmd.Stdout = &out
    err := cmd.Run()
    if err != nil {
      log.Fatal(err)
    }
    fmt.Println(out.String())
  } else {
    log.Fatal(err)
  }
}
