#!/bin/sh

# options: --lex
if [ "$#" -lt 1 ]; then
    echo Command line args: optional args filename
    exit 2 
elif test "$#" -gt 1; then
    option=$1
    sourceFile=$2
else
    sourceFile=$1
fi

# remove comments
outputStr=$(cat $sourceFile |
sed -nE '
#delete // comments
s/^(.*)\/\/.*$/\1/
#/* comments
/\/\*/{
    :append
    #if we do not see */
    /\*\//!{
        #its a multiline comment
        #eat next line
        n
        #check now
        b append
    }
    d
}
p')

stack --stack-yaml /Users/stephaniemerino/Downloads/projects/compiler/my-project/stack.yaml build

if [ "$?" -ne 0 ]; then
    echo "Error: stack build failed."
    exit -1
fi

path_to_lexer_exe="/Users/stephaniemerino/Downloads/projects/compiler/\
my-project/.stack-work/dist/x86_64-osx/ghc-9.8.4/build/Lexer/lexer"

path_to_parser_exe="/Users/stephaniemerino/Downloads/projects/\
compiler/my-project/.stack-work/dist/x86_64-osx/ghc-9.8.4/build/Parser/parser"

if [ $1 == "--lex" ] || [ $1 == "--parse" ]; then
    lex=$(echo $outputStr | stack exec $path_to_lexer_exe)
    status=$?

    if  [ $1 == "--lex" ]; then
        echo $lex
    fi

    if [ $status -ne 0 ]; then
        echo "Error: lexer failed."
        exit -1
    fi
fi

if [[ $1 == "--parse" ]]; then
    #need newlines so $lex is in quotes
    parser=$(echo "$lex" | stack exec $path_to_parser_exe)
    status=$?
    echo $lex
    echo "Parsing..."
    echo $parser
    if [ $status -ne 0 ]; then
        echo "Error: parser failed."
        exit -1
    fi
fi

exit 0
