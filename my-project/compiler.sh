#!/bin/sh

: 'Command line args: --lex --parse --codegen filename'

# options: --lex
if [ "$#" -lt 1 ]; then
    echo "Command line args: --lex --parse --codegen filename (required)"
    exit 2
elif test "$#" -gt 1; then
    option=$1
    sourceFile=$2
else
    sourceFile=$1
fi

stack --stack-yaml /Users/stephaniemerino/Downloads/projects/compiler/my-project/stack.yaml build

if [ "$?" -ne 0 ]; then
    echo "Error: stack build failed."
    exit -1
fi

input=$(gcc -E -P $sourceFile)

pathToGeneratedExe="/Users/stephaniemerino/Downloads/projects/compiler/\
my-project/.stack-work/dist/x86_64-osx/ghc-9.8.4/build/Main/Main"

if [[ $option == "--lex" ]]; then
    echo $input | stack exec $pathToGeneratedExe lex
elif [[ $option == "--parse" ]]; then
    echo $input | stack exec $pathToGeneratedExe parse
elif [[ $option == "--codegen" ]]; then
    echo $input | stack exec $pathToGeneratedExe codegen
else
    echo $input | stack exec $pathToGeneratedExe
fi
