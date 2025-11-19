: 'TODO: switching up the arguments should still work'

function callCompiler() {
    local option=$1
    local filename=$2
    printf "\n"
    ./compiler.sh $stage $filename
    printf "====================\n"
}

if [ "$#" -eq 0 ]; then
    echo "Command line args: chapter_N (required) --S --lex --codegen --parse --tacky (optional) --extra-credit"
    exit 2
fi

extra=0
for arg in "$@"; do
    if [[ "$arg" == "--extra-credit" ]]; then
        extra=1
    elif [[ "$arg" == *"chapter"* ]]; then
        chapter=$arg 
    else
        stage=$arg
    fi
done

#echo $extra $chapter $stage

filepath=/Users/stephaniemerino/Downloads/writing-a-c-compiler-tests/tests/$chapter
for file in "$filepath"/* # chapter folder
do 
    # valid or invalid folders
    for testFile in "$file"/* 
    do 
        if [ -d "$testFile" ]; then
            for extraCredit in "$testFile"/*
            do
                # printing chapter_N/valid or invalid/filename.c
                echo $extraCredit | sed -E 's/.*tests\/(.*)/\1/'
                printf "\n"
                if [[ $extraCredit =~ .c$ ]]; then
                    # printing out contents of filename.c
                    cat $extraCredit
                    callCompiler $stage $extraCredit
                fi
            done
        fi
        # printing chapter_N/valid or invalid/filename.c
        echo $testFile | sed -E 's/.*tests\/(.*)/\1/'
        printf "\n"
        if [[ $testFile =~ .c$ ]]; then
            # printing out contents of filename.c
            cat $testFile
            callCompiler $stage $testFile
        fi
    done
done
