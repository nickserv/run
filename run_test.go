package main

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"

  "testing"
)

func Test(t *testing.T) {
  RegisterFailHandler(Fail)
  RunSpecs(t, "Run")
}

var _ = Describe("Run", func() {
  It("has a version number", func() {
    Expect(Version).ToNot(BeNil())
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
        command, err := start([]string{"run", "hello.rb"})
        Expect(command).To(Equal("ruby hello.rb"))
        Expect(err).ToNot(HaveOccurred())
      })
    })

    Context("when a filename is given without a known extension", func() {
      It("should return an error", func() {
        _, err := start([]string{"run", "hello.unknown"})
        Expect(err).To(HaveOccurred())
      })
    })

    Context("when a filename is given without any extension", func() {
      It("should return an error", func() {
        _, err := start([]string{"run", "hello"})
        Expect(err).To(HaveOccurred())
      })
    })

    Context("when no filename is given", func() {
      It("should return an error", func() {
        _, err := start([]string{"run"})
        Expect(err).To(HaveOccurred())
      })
    })
  })
})
