#!/bin/sh

include="/usr/include/csmith"

command -v bin/lacc >/dev/null 2>&1 || {
	echo "bin/lacc required, run 'make'"
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
		echo "Failed to compile with gcc"
		exit 1
	fi

	timeout 1 "$program" > "csmith/gcc.txt"
	if [ $? -ne 0 ]; then
		rm "$program"
		continue
	fi

	clang -w -std=c99 -I "$include" "$filename" -o "$program"
	if [ $? -ne 0 ]; then
		echo "Failed to compile with clang"
		exit 1
	fi

	timeout 1 "$program" > "csmith/clang.txt"
	if [ $? -ne 0 ]; then
		echo "Failed to run with clang"
		continue
	fi

	diff "csmith/gcc.txt" "csmith/clang.txt"
	if [ $? -ne 0 ]; then
		echo "Skipping test because gcc and clang differ"
		continue
	fi

	rm "$program"
	./check.sh \
		"bin/lacc -w -std=c99 -I ${include}" "$filename" \
		"gcc -w -std=c99 -I ${include}"
	if [ $? -ne 0 ]; then
		break
	fi

	n=$((n + 1))
done

# Run test case with both compilers, and print first diff line. This
# reveals which variable is the first to create a mismatched checksum.
#
# Manual cleanup of test.c is required before running creduce. Remove
# checksum calculation of other variables, and fix lines that produce
# warnings.
#
# ./check.sh "bin/lacc -std=c99" creduce/test.c "cc -std=c99" 2> foo.out
#
# Modify creduce/interesting.sh to contain the new values for expected
# and actual checksum, provided as input script to creduce.

mkdir -p creduce
bin/lacc -std=c99 -I "$include" -w -E "$filename" -o creduce/test.c
bin/lacc -std=c99 -I "$include" -w "$filename" -o creduce/test -lm
gcc -std=c99 -I "$include" "$filename" -o creduce/test-cc
cp interesting.sh creduce
creduce/test 1 > creduce/lacc.out && creduce/test-cc 1 > creduce/cc.out
diff --side-by-side --suppress-common-lines creduce/lacc.out creduce/cc.out | head -n 1
