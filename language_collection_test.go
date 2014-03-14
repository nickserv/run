package main

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("languageCollection", func() {
  Describe(".commandForFile", func() {
    Context("when a filename is given with a known extension", func() {
      It("should be a valid command", func() {
        command, err := defaultLanguages.commandForFile("hello.rb")
        Expect(command).To(Equal("ruby hello.rb"))
        Expect(err).ToNot(HaveOccurred())
      })
    })

    Context("when a filename is given without a known extension", func() {
      It("should return an error", func() {
        _, err := defaultLanguages.commandForFile("hello.unknown")
        Expect(err).To(HaveOccurred())
      })
    })

    Context("when a filename is given without any extension", func() {
      It("should return an error", func() {
        _, err := defaultLanguages.commandForFile("hello")
        Expect(err).To(HaveOccurred())
      })
    })
  })

  Describe(".merge", func() {
    It("should successfully merge two languageCollections", func() {
      languages := languageCollection {
        "a": "1",
        "b": "nope",
      }

      otherLanguages := languageCollection {
        "b": "2",
        "c": "3",
      }

      expectedLanguages := languageCollection {
        "a": "1",
        "b": "2",
        "c": "3",
      }

      languages.merge(otherLanguages)
      Expect(languages).To(Equal(expectedLanguages))
    })
  })
})