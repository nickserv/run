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
      expectedLanguages := languageCollection {
        "uno": language{"one", "two"},
        "dos": language{"three", "four"},
      }
      Expect(languages).To(Equal(expectedLanguages));
      Expect(err).ToNot(HaveOccurred())
    })
  })
})
