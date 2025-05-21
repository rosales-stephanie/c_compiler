
filepath=/Users/stephaniemerino/Downloads/writing-a-c-compiler-tests/tests/chapter_2/invalid_parse
for file in "$filepath"/*
do 
    ./compiler.sh --lex $file
    printf "\n--------------------\n"
    cat $file
    printf "\n====================\n"
done
