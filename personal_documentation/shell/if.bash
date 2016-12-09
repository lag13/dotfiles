#!/bin/bash

# variable is equal to 10
v=10
if [ "$v" = 10 ]
then
    echo yep
fi

# https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html

# variable has a non-zero length
if [ "$v" ]
# if [ -n "$v" ]
then
    echo "v has a non-zero length"
fi

# in reality, an if statement in bash is always given a bash command ("[" is
# just an alias for the "test" command), when the command returns a '0' exit
# code (so everything was successfull) then the first branch is taken.
if cat fileDoesNotExist
then
    echo file was catted
else
    echo file was NOT catted
fi
