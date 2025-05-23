: 'Command line args: chapter_N --lex --parse'

# options: --lex
if [ "$#" -eq 0 ]; then
    echo "Command line args: chapter_N --lex --parse"
    exit 2
elif test "$#" -eq 2; then
    chapter=$1
    stage=$2
elif test "$#" -eq 1; then
    chapter=$1
else
    echo "Command line args: chapter_N"
    exit 2
fi

filepath=/Users/stephaniemerino/Downloads/writing-a-c-compiler-tests/tests/$chapter
for file in "$filepath"/*
do 
    for testFile in "$file"/*
    do 
        if test "$#" -eq 2; then
            ./compiler.sh $stage $testFile
        else
            ./compiler.sh $testFile
        fi
        echo $testFile | sed -E 's/.*tests\/(.*)/\1/'
        printf "\n"
        cat $testFile
        printf "\n====================\n"
    done
done
