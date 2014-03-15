package main

import(
  "fmt"
  "path/filepath"
  "strings"
)

// commandForExtension returns the command template used to execute a file with
// a given extension. The second return value is true if and only if an
// appropriate command is found.
func (languages languageCollection) commandForExtension(extension string) (string, bool) {
  for _, language := range languages {
    if language.Extension == extension {
      return language.Command, true
    }
  }
  return "", false
}

// commandForFile returns the command that should be used to run the given file.
// The beginning of the command depends on the extension of the file, while the
// file path portion(s) of the command will automatically be substituted with
// the given file path.
func (languages languageCollection) commandForFile(path string) (string, error) {
  extension := strings.Replace(filepath.Ext(path), ".", "", -1)

  // Fill out the command template.
  if command, success := languages.commandForExtension(extension); success {
    return strings.Replace(command, "%", path, -1), nil
  }
  return "", fmt.Errorf("run %s: could not determine how to run the file because \"%s\" is not a known extension", path, extension)
}

// merge merges the contents of map2 into map1, using map1 as the default
// values.
func (languages languageCollection) merge(otherLanguages languageCollection) {
  for key, value := range otherLanguages {
    languages[key] = value
  }
}

// printLanguages prints the given language collection in the following format.
//
//   extension: command
//   extension: command
func (languages languageCollection) string() string {
  str := ""
  for extension, command := range languages {
    str = str + fmt.Sprintf("%s: %s\n", extension, command)
  }
  return str;
}
