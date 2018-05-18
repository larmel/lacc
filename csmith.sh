#!/bin/sh

include="/usr/include/csmith"

command -v lacc >/dev/null 2>&1 || {
	echo "lacc required, run 'make install'"
	exit 1
}

command -v csmith >/dev/null 2>&1 || {
	echo >&2 "Please install csmith"
	exit 1
}

mkdir -p csmith

n=1
while [ true ]
do
	filename="csmith/${n}.c"
	program="csmith/${n}"
	csmith --no-packed-struct --float > "$filename"

	gcc -w -std=c99 -I "$include" "$filename" -o "$program"
	if [ $? -ne 0 ]; then
		exit 1
	fi

	timeout 1 "$program" > /dev/null
	if [ $? -ne 0 ]; then
		rm "$program"
		continue
	fi

	rm "$program"
	./check.sh \
		"lacc -w -std=c99 -I ${include}" "$filename" \
		"gcc -w -std=c99 -I ${include}"
	if [ $? -ne 0 ]; then
		break
	fi

	n=$((n + 1))
done

# Run test case with both compilers, and print first diff line. This
# reveals which variable is the first to create a mismatched checksum.
#
# Manual cleanup of reduce.c is required before running creduce. Remove
# checksum calculation of other variables, and fix lines that produce
# warnings.
#
# ./check.sh "lacc -std=c99" creduce/reduce.c "gcc -std=c99" 2> foo.out
#
# Modify creduce/creduce.sh to contain the new values for expected
# and actual checksum, provided as input script to creduce.

mkdir -p creduce
lacc -std=c99 -I "$include" -w -E "$filename" -o creduce/reduce.c
lacc -std=c99 -I "$include" -w -c "$filename" -o creduce/reduce.o
gcc creduce/reduce.o -o creduce/reduce -lm
gcc -std=c99 -I "$include" "$filename" -o creduce/reduce-cc
cp creduce.sh creduce/
creduce/reduce 1 > creduce/lacc.out && creduce/reduce-cc 1 > creduce/cc.out
diff --side-by-side --suppress-common-lines creduce/lacc.out creduce/cc.out | head -n 1
