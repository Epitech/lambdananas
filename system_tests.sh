#!/bin/sh

exit_status=0

if ! command -v shelltest &> /dev/null
then
    echo "shelltestrunner could not be found"
    echo "make sure the executable is in your PATH"
    echo "you can download it using 'stack install shelltestrunner-1.9'"
    exit 1
else
    echo "shelltest will run on all .test files of the test/ directory"
    stack build --exec "shelltest test/"
    exit_status=$?
    rm -f style-minor.txt style-major.txt style-info.txt style-student.txt banned_funcs > /dev/null
    exit $exit_status
fi
