package main

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"

  "path"
)

var _ = Describe("Run", func() {
  It("has a version number", func() {
    Expect(version).ToNot(BeNil())
  })

  Describe(".getLanguages", func() {
    It("should properly parse a JSON config file", func() {
      languages, err := getLanguages(path.Join(callerDir(), "mock_commands.json"))
      expectedLanguages := languageCollection { "one": "two", "three": "four" }
      Expect(languages).To(Equal(expectedLanguages));
      Expect(err).ToNot(HaveOccurred())
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
