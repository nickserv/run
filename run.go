package main

import (
  "bytes"
  "encoding/json"
  "errors"
  "fmt"
  "io/ioutil"
  "log"
  "os"
  "path/filepath"
  "strings"
)

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

func start(args []string) error {
  if len(args) <= 1 {
    return errors.New("no files given")
  }
  command, err := commandForFile(args[1])
  if err != nil {
    return err
  }
  fmt.Println(command)
  return nil
}

func main() {
  if err := start(os.Args); err != nil {
    log.Fatal(err)
  }
}
