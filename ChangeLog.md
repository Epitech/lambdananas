# Changelog for haskell-style-checker

## 2.0.0.0

### Features

- Added Vera compatibility option
- Added Argos compatibility option
- Changed input methods now taking a list of directories and files at the end of cli options and if none, reads from stdin
- Added style issues gravity

### Miscellaneous

- Deprecated old cli options parser for optparse-applicative
- Added basic barebone system tests
- Changed CI to create a multi-job workflow
- Added documentation to a good part of the code

## 2.1.0.0

### Features

- Added dumping manifest option (--dump-manifest)

### Miscellaneous

- Update CI artifact name
- Cleaner code

## 2.2.0.0

### Features

- Added more precise parse errors
- Added dumping parse errors (extension and syntax) to vera logs and argos files
- Added dumping a vague output to style-student.txt when argos output is enabled (experimental !)

### Miscellaneous

- Update docs & tests

## 2.3.0.0

### Features

- Added excluding directories (be sure to read how it works with --help, it could not work as you think)

### Miscellaneous

- Update code structure to allow for easier add of new Issues and Warn creation

## 2.3.1.0

### Fixes

- Fix Warn formatting regressions due to broken CI
- Fix vague output creating a style-student.txt file even when there is no errors

