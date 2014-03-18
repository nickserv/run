package main

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("languageCollection", func() {
  Describe(".commandForExtension", func() {
    Context("when a known extension is given", func() {
      It("should return the appropriate command", func() {
        command, err := defaultLanguages.commandForExtension("rb")
        Expect(command).To(Equal("ruby %"))
        Expect(err).ToNot(HaveOccurred())
      })
    })

    Context("when an unknown extension is given", func() {
      It("should return an error", func() {
        _, err := defaultLanguages.commandForExtension("unknown")
        Expect(err).To(HaveOccurred())
      })
    })
  })

  Describe(".commandForLanguage", func() {
    Context("when a known language is given", func() {
      It("should return the appropriate command", func() {
        command, err := defaultLanguages.commandForLanguage("ruby")
        Expect(command).To(Equal("ruby %"))
        Expect(err).ToNot(HaveOccurred())
      })
    })

    Context("when an unknown extension is given", func() {
      It("should return an error", func() {
        _, err := defaultLanguages.commandForLanguage("unknown")
        Expect(err).To(HaveOccurred())
      })
    })
  })

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

  Describe(".commandForFileAndLanguage", func() {
    Context("when a filename is given with a known language", func() {
      It("should be a valid command", func() {
        command, err := defaultLanguages.commandForFileAndLanguage("hello", "ruby")
        Expect(command).To(Equal("ruby hello"))
        Expect(err).ToNot(HaveOccurred())
      })
    })

    Context("when a filename is given without a known language", func() {
      It("should return an error", func() {
        _, err := defaultLanguages.commandForFileAndLanguage("hello", "english")
        Expect(err).To(HaveOccurred())
      })
    })
  })

  Describe(".merge", func() {
    It("should successfully merge two languageCollections", func() {
      languages := languageCollection {
        "x": language{"a", "1"},
        "y": language{"b", "nope"},
      }

      otherLanguages := languageCollection {
        "y": language{"b", "2"},
        "z": language{"c", "3"},
      }

      expectedLanguages := languageCollection {
        "x": language{"a", "1"},
        "y": language{"b", "2"},
        "z": language{"c", "3"},
      }

      languages.merge(otherLanguages)
      Expect(languages).To(Equal(expectedLanguages))
    })
  })

  Describe(".string", func() {
    It("should show a correct string representation of the languageCollection", func() {
      languages := languageCollection {
        "x": language{"a", "1"},
        "y": language{"b", "2"},
      }
      Expect(languages.string()).To(Equal("x: 1\ny: 2\n"))
    })
  })
})
