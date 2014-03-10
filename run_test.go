package main

import (
  . "github.com/franela/goblin"

  "errors"
  "testing"
)

func Test(t *testing.T) {
  g := Goblin(t)

  g.Describe("Run", func() {
    extensionError := errors.New("run could not determine how to run this file because it does not have a known extension")

    g.Describe(".command_for_file", func() {
      g.Describe("when a filename is given with a known extension", func() {
        g.It("should be a valid command", func() {
          command, err := commandForFile("hello.rb")
          g.Assert(command).Equal("ruby hello.rb")
          g.Assert(err).Equal(nil)
        })
      })

      g.Describe("when a filename is given without a known extension", func() {
        g.It("should return an error", func() {
          _, err := commandForFile("hello.unknown")
          g.Assert(err).Equal(extensionError)
        })
      })

      g.Describe("when a filename is given without any extension", func() {
        g.It("should return an error", func() {
          _, err := commandForFile("hello")
          g.Assert(err).Equal(extensionError)
        })
      })
    })
  })
}
