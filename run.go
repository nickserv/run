package main

import (
  "bytes"
  "encoding/json"
  "fmt"
  "io/ioutil"
  "log"
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

func main() {
  commands, err := getCommands()
  if err != nil {
    log.Fatal(err)
  }
  fmt.Println(commands["rb"])
}
