
filepath=/Users/stephaniemerino/Downloads/writing-a-c-compiler-tests/tests/chapter_1/valid
for file in "$filepath"/*
do 
    cat $file
    printf "\n"
    ./compiler.sh $file
    printf "\n====================\n"
done
