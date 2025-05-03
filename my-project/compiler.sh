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

stack build 

if [[ $1 == "--lex" ]]; then
    lex=$(echo $outputStr | stack exec lexer)
    echo $lex
elif [[ $1 == "--parse" ]]; then
    parser=$(echo $outputStr | stack exec lexer | stack exec parser)
    echo $parser
else
    exit 2
fi

exit $?
