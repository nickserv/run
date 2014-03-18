package main

import(
  "fmt"
  "path/filepath"
  "strings"
)

// A collection of information about different languages. This is represented as
// a map from language names (strings) to language structs.
type languageCollection map[string]language

// A language struct represents the information that Run knows about any
// executable language. Extension represents the main file extension for files
// written in this language. Command represents the command template used to run
// files of this type, with a "%" representing the appropriate filename.
type language struct {
  Extension string
  Command string
}

// commandForExtension returns the command template used to execute a file with
// a given extension.
func (languages languageCollection) commandForExtension(extension string) (string, error) {
  for _, language := range languages {
    if language.Extension == extension {
      return language.Command, nil
    }
  }
  return "", fmt.Errorf(`run: could not determine how to run the file because "%s" is not a known extension`, extension)
}

// commandForLanguage returns the command template used to execute a file
// written in a given language.
func (languages languageCollection) commandForLanguage(languageName string) (string, error) {
  if language, success := languages[languageName]; success {
    return language.Command, nil
  }
  return "", fmt.Errorf(`run: could not determine how to run the file because "%s" is not a known language`, languageName)
}

// commandForFile returns the command that should be used to run the given file.
// The beginning of the command depends on the extension of the file, while the
// file path portion(s) of the command will automatically be substituted with
// the given file path.
func (languages languageCollection) commandForFile(path string) (string, error) {
  extension := strings.Replace(filepath.Ext(path), ".", "", -1)

  // Fill out the command template.
  command, err := languages.commandForExtension(extension)
  return strings.Replace(command, "%", path, -1), err
}

// commandForFileAndLanguage returns the command that should be used to run the
// given file with the given language. The beginning of the command depends on
// the extension of the file, while the file path portion(s) of the command will
// automatically be substituted with the given file path.
func (languages languageCollection) commandForFileAndLanguage(path string, languageName string) (string, error) {
  // Fill out the command template.
  command, err := languages.commandForLanguage(languageName)
  return strings.Replace(command, "%", path, -1), err
}

// merge merges the contents of map2 into map1, using map1 as the default
// values.
func (languages languageCollection) merge(otherLanguages languageCollection) {
  for key, value := range otherLanguages {
    languages[key] = value
  }
}

// String shows a string representation of the given language collection in the
// following format.
//
//   language: command
//   language: command
func (languages languageCollection) String() string {
  str := ""
  for extension, language := range languages {
    str = str + fmt.Sprintf("%s: %s\n", extension, language.Command)
  }
  return str;
}
