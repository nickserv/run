package main

import (
  "bytes"
  "encoding/json"
  "fmt"
  "io/ioutil"
  "log"
)

type Commands map[string]string

func main() {
  jsonStream, err := ioutil.ReadFile("./commands.json")
  if err != nil {
    log.Fatal(err)
  }
  decoder := json.NewDecoder(bytes.NewReader(jsonStream))

  var commands Commands
  if err := decoder.Decode(&commands); err != nil {
    log.Fatal(err)
  }
  fmt.Println(commands["rb"])
}
