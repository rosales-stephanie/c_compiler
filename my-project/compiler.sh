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

if [[ $1 == "--lex" ]]; then
    lex=$(echo $outputStr | stack exec /Users/stephaniemerino/Downloads/projects/compiler/my-project/.stack-work/dist/x86_64-osx/ghc-9.8.4/build/Lexer/lexer)
    status=$?
    echo $lex
elif [[ $1 == "--parse" ]]; then
    parser=$(echo $outputStr | stack exec lexer | stack exec parser)
    status=$?
    echo $parser
else
    exit 2
fi

exit $status
