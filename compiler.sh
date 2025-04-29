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
    echo $outputStr | \
    runhaskell -i/Users/stephaniemerino/Downloads/projects/compiler/ \
    /Users/stephaniemerino/Downloads/projects/compiler/lexer.hs
elif [[ $1 == "--parse" ]]; then
    lex=$(echo $outputStr | \
    runhaskell -i/Users/stephaniemerino/Downloads/projects/compiler/ \
    /Users/stephaniemerino/Downloads/projects/compiler/lexer.hs)
    echo "$lex" | \
    runhaskell -i/Users/stephaniemerino/Downloads/projects/compiler/ \
    /Users/stephaniemerino/Downloads/projects/compiler/parser.hs
else
    exit 2
fi

exit $?
