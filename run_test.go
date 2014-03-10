package main

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"

  "errors"
  "testing"
)

func Test(t *testing.T) {
  RegisterFailHandler(Fail)
  RunSpecs(t, "Run")
}

var _ = Describe("Run", func() {
  extensionError := errors.New("run could not determine how to run this file because it does not have a known extension")

  Describe(".command_for_file", func() {
    Context("when a filename is given with a known extension", func() {
      It("should be a valid command", func() {
        command, err := commandForFile("hello.rb")
        Expect(command).To(Equal("ruby hello.rb"))
        Expect(err).To(BeNil())
      })
    })

    Context("when a filename is given without a known extension", func() {
      It("should return an error", func() {
        _, err := commandForFile("hello.unknown")
        Expect(err).To(Equal(extensionError))
      })
    })

    Context("when a filename is given without any extension", func() {
      It("should return an error", func() {
        _, err := commandForFile("hello")
        Expect(err).To(Equal(extensionError))
      })
    })
  })
})
