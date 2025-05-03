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

if [[ $1 == "--lex" ]]; then
    echo $outputStr | lex=$(runhaskell app/lexer.hs)
    echo $lex
elif [[ $1 == "--parse" ]]; then
    lex=$(echo $outputStr | \
    runhaskell -i/Users/stephaniemerino/Downloads/projects/compiler/compiler/app \
    /Users/stephaniemerino/Downloads/projects/compiler/compiler/app/lexer.hs)
    echo "$lex" | \
    cabal run 
else
    exit 2
fi

exit $?
