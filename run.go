package main

import (
  "bytes"
  "encoding/json"
  "errors"
  "fmt"
  "io/ioutil"
  "log"
  "os"
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

func start(args []string) error {
  if len(args) <= 1 {
    return errors.New("No files given.")
  }
  commands, err := getCommands()
  if err != nil {
    log.Fatal(err)
  }
  fmt.Println(commands[args[1]])
  return nil
}

func main() {
  if err := start(os.Args); err != nil {
    log.Fatal(err)
  }
}
