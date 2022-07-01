#!/bin/sh

exit_status=1

if command -v shelltest
then
    echo "shelltest will run on all .test files of the test/ directory"
    stack install --local-bin-path .
    shelltest -j 4 test/
    exit_status=$?
    rm -f style-minor.txt \
      style-major.txt \
      style-info.txt \
      style-student.txt \
      lambdananas-exe \
      banned_funcs > /dev/null
    exit $exit_status
else
    echo "shelltestrunner could not be found"
    echo "make sure the executable is in your PATH"
    echo "you can download it using 'stack install shelltestrunner-1.9'"
    exit $exit_status
fi
