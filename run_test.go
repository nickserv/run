package main

import (
  "testing"
  . "github.com/franela/goblin"
)

func Test(t *testing.T) {
  g := Goblin(t)

  g.Describe("Run", func() {
    g.Describe(".command_for_file", func() {
      g.Describe("when a filename is given with a known extension", func() {
        g.It("should be a valid command", func() {
          g.Assert(Run.command_for_file("hello.rb")).Equal("ruby hello.rb")
        })
      })

      g.Describe("when a filename is given without a known extension", func() {
        g.It("should be nil", func() {
          g.Assert(Run.command_for_file("hello.unknown")).Equal(nil)
        })
      })

      g.Describe("when a filename is given without any extension", func() {
        g.It("should be nil", func() {
          g.Assert(Run.command_for_file("hello")).Equal(nil)
        })
      })
    })
  })
}
