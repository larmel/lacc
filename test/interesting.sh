#!/bin/sh

# Interestingness script, to be supplied creduce together with the file
# being reduced. Should return 0 if case is interesting, non-zero
# otherwise.
#
# creduce ./interesting.sh test.c
#
# We are interested in catching wrong code generation bugs, which are
# the hardest to get to the bottom to from just reading csmith generated
# code.
#
# For best results, remove any unnecessary crc calculation for variables
# that do not contribute to the wrong result.

filename="test.c"

# Replace these values, with output from correct compilation and faulty
# lacc compilation.
expect="checksum = 83CE8DDE"
actual="checksum = ADF8FC99"

# Give up after a couple of seconds. Reduced code can contain infinite
# loops.
ulimit -t 2

# See that the file is still compilable. Make sure it has no screaming
# warnings to avoid some undefined behavior.
gcc -std=c99 "$filename" -o test > /dev/null 2> error.txt && ! grep "warning" error.txt
if [ $? -ne 0 ]
then
	exit 1
fi

# Get expected output from cc
./test > test.txt && grep "$expect" test.txt > /dev/null 2>&1
if [ $? -ne 0 ]
then
	exit 1
fi

# Get expected output from clang also, to avoid some cases of undefined
# behavior that may be treated the same for gcc and lacc.
clang -std=c99 -w "$filename" -o test > /dev/null
./test > test.txt && grep "$expect" test.txt > /dev/null 2>&1
if [ $? -ne 0 ]
then
	exit 1
fi

# Try to compile with lacc
lacc -std=c99 "$filename" -o test -lm > /dev/null
if [ $? -ne 0 ]
then
	exit 1
fi

# Get output from lacc
./test > test.txt && grep "$actual" test.txt > /dev/null 2>&1
