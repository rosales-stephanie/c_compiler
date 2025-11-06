if [ "$#" -eq 0 ]; then
    echo "Command line args: chapter_number (required) S lex codegen parse tacky (required)" 
    exit 2
elif test "$#" -eq 2; then
    chapter=$1
    stage=$2
elif test "$#" -eq 1; then
    chapter=$1
fi
cd /Users/stephaniemerino/Downloads/writing-a-c-compiler-tests
./test_compiler /Users/stephaniemerino/Downloads/projects/compiler/my-project/compiler.sh --chapter $chapter --stage $stage
cd /Users/stephaniemerino/Downloads/projects/compiler/my-project/
