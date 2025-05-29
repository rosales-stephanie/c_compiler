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

# gcc -E -P sourceFile.c -o sourceFile.i
preProcessedFile=$(gcc -E -P $sourceFile)

pathToGeneratedExe="/Users/stephaniemerino/Downloads/projects/compiler/\
my-project/.stack-work/dist/x86_64-osx/ghc-9.8.4/build/Main/Main"

if [[ $option == "--lex" ]]; then
    echo $preProcessedFile | stack exec $pathToGeneratedExe lex
elif [[ $option == "--parse" ]]; then
    echo $preProcessedFile | stack exec $pathToGeneratedExe parse
elif [[ $option == "--tacky" ]]; then
    echo $preProcessedFile | stack exec $pathToGeneratedExe tacky
elif [[ $option == "--codegen" ]]; then
    echo $preProcessedFile | stack exec $pathToGeneratedExe codegen
elif [[ $option == "--S" ]]; then
    #output an assembly file with a .s extension
    #but do not assemble or link it
    echo $preProcessedFile | stack exec $pathToGeneratedExe
else
    #output an assembly file with a .s extension (assemblyFile.s)
    echo $preProcessedFile | stack exec $pathToGeneratedExe $sourceFile
    if [ "$?" -ne 0 ]; then
        exit -1
    fi
    assemblyFile=$(echo $sourceFile | sed 's/.c$/.s/')
    outputFile=$(echo $sourceFile | sed 's/.c$//')
    #assemble and link the file to produce an executable 
    gcc $assemblyFile -o $outputFile
    #then delete the assembly file
    rm $assemblyFile
    exit 0
fi
