#!/usr/bin/env bash

if ! command -v shelltest &> /dev/null
then
    echo "shelltestrunner could not be found"
    echo "make sure the executable is in your PATH"
    echo "you can download it using 'stack install shelltestrunner-1.9'"
    exit
else
    echo "shelltest will run on all .test files of the test/ directory"
    shelltest test/
    rm -f style-minor.txt style-major.txt style-info.txt banned_funcs > /dev/null
fi
