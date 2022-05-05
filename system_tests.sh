#!/usr/bin/env bash

if ! command -v shelltest &> /dev/null
then
    echo "shelltestrunner could not be found"
    echo "make sure the executable is in your PATH"
    echo "you can download it using 'stack install shelltestrunner-1.9'"
    exit
else
    echo "making sure binary is built"
    stack build
    sleep 0.2 # stack does stuff even after exit so this is a dirty fix
    echo "shelltest will run on all .test files of the test/ directory"
    shelltest -c test/
fi
