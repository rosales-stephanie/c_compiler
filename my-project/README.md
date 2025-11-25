# How I started:
stack new my-project
cd my-project

# To build project and check for errors just do:
stack build

# To run/test use:
./compiler.sh - runs one c file
./runTests.sh - runs chapter's tests
./testCompiler.sh - runs test-compiler test suite

# To run GHC's interactive mode open your terminal and type in ghci like so:
ghci
# To load and test functions in ghci
:l src/lexer.hs
