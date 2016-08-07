#!/bin/bash

# Interestingness script, to be supplied creduce together with the file
# being reduced. Should return 0 if case is interesting, non-zero
# otherwise.
#
# creduce -n 8 ./creduce.sh reduce.c
#
# We are interested in catching wrong code generation bugs, which are
# the hardest to get to the bottom to from just reading csmith generated
# code. Copy failing test to separate creduce directory, and hard code
# the expected output from successful and buggy compilation.
#
# For best results, remove any unnecessary crc calculation for variables
# that do not contribute to the wrong result.

filename="reduce.c"

# Replace these values, with output from correct compilation and faulty
# lacc compilation.
expect="checksum = F2A1C4A9"
actual="checksum = 3E0BC437"

# Give up after a couple of seconds. Reduced code can contain infinite
# loops.
ulimit -t 2

# See that the file is still compilable. Make sure it has no screaming
# warnings to avoid some undefined behavior.
gcc -std=c99 ${filename} -o reduce > /dev/null 2> error.txt && ! grep "warning" error.txt
if [ $? -ne 0 ]
then
	exit 1
fi

# Get expected output from cc
./reduce > reduce.txt && grep "${expect}" reduce.txt > /dev/null 2>&1
if [ $? -ne 0 ]
then
	exit 1
fi

# Get expected output from clang also, to avoid some cases of undefined
# behavior that may be treated the same for gcc and lacc.
clang -std=c99 -w ${filename} -o reduce > /dev/null
./reduce > reduce.txt && grep "${expect}" reduce.txt > /dev/null 2>&1
if [ $? -ne 0 ]
then
	exit 1
fi

# Try to compile with lacc
lacc -std=c99 -c ${filename} -o reduce.o > /dev/null && cc reduce.o -o reduce -lm  > /dev/null
if [ $? -ne 0 ]
then
	exit 1
fi

# Get output from lacc
./reduce > reduce.txt && grep "${actual}" reduce.txt > /dev/null 2>&1
