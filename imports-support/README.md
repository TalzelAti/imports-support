# imports-support

### usage
## cli single formatting
## cli multiple packages in directory
## watcher on path

## actions (to be) implemented
* update cabal from src
* * reads each src imports
* * creates a packages list from each src
* * updates cabal with unified list from all srcs
* update stack from src
* lift modules to package imports (add the header, and labels to all imports)

## TODO

### tests
- write tests for parser
- write tests for formatter

### v0

- fix formatter error
- add support for ((functions)) and data(..)
- refactor run cmd
- add better control flags.
-- delete-temp: delete temp files
- todo add option to modify haskell files
- add documantation

## v0.1

- add option to annotate and remove redundant packages from package.yml
- make files explicitly packageImports
- make files explicitly unPackageImports
- deal with types imports (paren)


### long term
-- todo: add option to read errors from the compiler, and run only when there are import errors
