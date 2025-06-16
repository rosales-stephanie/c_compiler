#!/bin/sh

# options: --lex
if [ "$#" -lt 1 ]; then
    echo Command line args: --lex --S --parse --codegen --tacky filename.c
    exit 2
elif [ "$#" -eq 2 ]; then
    if [[ $1 =~ -- ]]; then
        option=$1
    else 
        option=$2
    fi
fi
if [ "$#" -eq 2 ]; then
    if [[ $1 =~ -- ]]; then
        sourceFile=$2
    else
        sourceFile=$1
    fi
elif [ "$#" -eq 1 ]; then
    sourceFile=$1
    option="--S"
fi

if ! [[ $sourceFile =~ .c$ ]]; then
    echo Command line args: --lex --S --parse --codegen --tacky filename.c
    echo Invalid source file: $sourceFile
    exit 2
fi 

stack --stack-yaml /Users/stephaniemerino/Downloads/projects/compiler/my-project/stack.yaml build

if [ "$?" -ne 0 ]; then
    echo "Error: stack build failed."
    exit -1
fi

preProcessedFile=$(echo $sourceFile | sed 's/.c$/.i/')
gcc -E -P $sourceFile -o $preProcessedFile

pathToGeneratedExe="/Users/stephaniemerino/Downloads/projects/compiler/\
my-project/.stack-work/dist/x86_64-osx/ghc-9.8.4/build/Main/Main"

stack exec $pathToGeneratedExe -- $option $preProcessedFile
if [ "$?" -ne 0 ]; then
    exit -1
fi
rm $preProcessedFile
if [ "$#" -eq 1 ]; then
    #output an assembly file with a .s extension (assemblyFile.s)
    assemblyFile=$(echo $sourceFile | sed 's/.c$/.s/')
    outputFile=$(echo $sourceFile | sed 's/.c$//')
    #assemble and link the file to produce an executable 
    gcc $assemblyFile -o $outputFile
    #delete the assembly file
    rm $assemblyFile
fi
