#!/bin/sh

include="/usr/include/csmith"

command -v bin/lacc >/dev/null 2>&1 || {
	echo "bin/lacc required, run 'make'."
	exit 1
}

command -v csmith >/dev/null 2>&1 || {
	echo >&2 "Please install csmith."
	exit 1
}

mkdir -p test/csmith bin/csmith

n=0
while [ true ]
do
	n=$((n + 1))
	filename="test/csmith/${n}.c"
	program="bin/csmith/program"
	if [ ! -f "${filename}" ]
	then
		csmith --no-packed-struct --float > "$filename"
	fi

	gcc -w -std=c99 -I "$include" "$filename" -o "$program"
	if [ $? -ne 0 ]
	then
		echo "Failed to compile with gcc"
		exit 1
	fi

	timeout 1 "$program" > "bin/csmith/gcc.txt"
	if [ $? -ne 0 ]
	then
		rm "$filename"
		continue
	fi

	clang -w -std=c99 -I "$include" "$filename" -o "$program"
	if [ $? -ne 0 ]
	then
		echo "Failed to compile with clang"
		exit 1
	fi

	timeout 1 "$program" > "bin/csmith/clang.txt"
	if [ $? -ne 0 ]
	then
		echo "Failed to run with clang"
		continue
	fi

	diff "bin/csmith/gcc.txt" "bin/csmith/clang.txt"
	if [ $? -ne 0 ]
	then
		echo "Skipping test because gcc and clang differ"
		continue
	fi

	test/check.sh \
		"bin/lacc -w -std=c99 -I ${include}" "$filename" \
		"gcc -w -std=c99 -I ${include}" \
		"bin/csmith"
	if [ $? -ne 0 ]
	then
		break
	fi
done

# Run test case with both compilers, and print first diff line. This
# reveals which variable is the first to create a mismatched checksum.
#
# Manual cleanup of test.c is required before running creduce. Remove
# checksum calculation of other variables, and fix lines that produce
# warnings.
#
# test/check.sh "bin/lacc -std=c99" test/creduce/test.c "cc -std=c99" 2> foo.out
#
# Modify creduce/interesting.sh to contain the new values for expected
# and actual checksum, provided as input script to creduce.

mkdir -p test/creduce
bin/lacc -std=c99 -I "$include" -w -E "$filename" -o test/creduce/test.c
bin/lacc -std=c99 -I "$include" -w "$filename" -o test/creduce/program -lm
gcc -std=c99 -I "$include" "$filename" -o test/creduce/reference
cp test/interesting.sh test/creduce
test/creduce/program 1 > test/creduce/program.out
test/creduce/reference 1 > test/creduce/reference.out
diff --side-by-side --suppress-common-lines \
	test/creduce/program.out test/creduce/reference.out | head -n 1
