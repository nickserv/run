package main

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"

  "testing"
)

// Set up the "go test" command.
func Test(t *testing.T) {
  RegisterFailHandler(Fail)
  RunSpecs(t, "Run")
}

// Implement the specs.
var _ = Describe("Run", func() {
  It("has a version number", func() {
    Expect(version).ToNot(BeNil())
  })

  Describe(".getCommands", func() {
    It("should get at least one command", func() {
      commands, err := getCommands()
      Expect(len(commands)).To(BeNumerically(">=", 1))
      Expect(err).ToNot(HaveOccurred())
    })
  })

  Describe(".commandForFile", func() {
    Context("when a filename is given with a known extension", func() {
      It("should be a valid command", func() {
        command, err := commandForFile("hello.rb")
        Expect(command).To(Equal("ruby hello.rb"))
        Expect(err).ToNot(HaveOccurred())
      })
    })

    Context("when a filename is given without a known extension", func() {
      It("should return an error", func() {
        _, err := commandForFile("hello.unknown")
        Expect(err).To(HaveOccurred())
      })
    })

    Context("when a filename is given without any extension", func() {
      It("should return an error", func() {
        _, err := commandForFile("hello")
        Expect(err).To(HaveOccurred())
      })
    })
  })

  Describe(".start", func() {
    Context("when a filename is given with a known extension", func() {
      It("runs the file", func() {
        command, err := start("run", "hello.rb")
        Expect(command).To(Equal("ruby hello.rb"))
        Expect(err).ToNot(HaveOccurred())
      })
    })

    Context("when a filename is given without a known extension", func() {
      It("should return an error", func() {
        _, err := start("run", "hello.unknown")
        Expect(err).To(HaveOccurred())
      })
    })

    Context("when a filename is given without any extension", func() {
      It("should return an error", func() {
        _, err := start("run", "hello")
        Expect(err).To(HaveOccurred())
      })
    })

    Context("when no filename is given", func() {
      It("should return an error", func() {
        _, err := start("run")
        Expect(err).To(HaveOccurred())
      })
    })
  })
})
