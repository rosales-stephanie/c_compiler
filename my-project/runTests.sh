# to do:
# overall - failed/success 
# show which files failed
# option chapter or chapter option
# run tests - everything up to chapter N
: 'Command line args: chapter_N --S --lex --codegen --parse --tacky'

if [ "$#" -eq 0 ]; then
    echo "Command line args: chapter_N (required) --S --lex --codegen --parse --tacky (optional)" 
    exit 2
elif test "$#" -eq 2; then
    chapter=$1
    stage=$2
elif test "$#" -eq 1; then
    chapter=$1
fi

filepath=/Users/stephaniemerino/Downloads/writing-a-c-compiler-tests/tests/$chapter
for file in "$filepath"/*
do 
    for testFile in "$file"/*
    do 
        echo "\n" $testFile | sed -E 's/.*tests\/(.*)/\1/'
        printf "\n"
        cat $testFile
        echo "\n"
        if test "$#" -eq 2; then
            ./compiler.sh $stage $testFile
        else
            ./compiler.sh $testFile
        fi
        printf "\n====================\n"
    done
done
