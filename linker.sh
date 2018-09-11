#!/bin/sh

lacc="$1"
if [ -z "$lacc" ]
then
	echo "Usage: $0 <compiler to test>";
	exit 1
fi

lacc="../../${lacc}"
cd test/linker/

# Target is always the same
cc foo.c bar.c -o a.cc.out
expected=$(./a.cc.out 1 2 3)

check()
{
	if [ $? -ne 0 ]
	then
		echo "$(tput setaf 1)Compilation failed!$(tput sgr0)";
		return 1
	fi

	if [ ! -f "$1" ]
	then
		echo "$(tput setaf 1)Did not create $1!$(tput sgr0)";
		return 1
	fi

	actual=$(LD_LIBRARY_PATH=. ./$1 1 2 3)

	if [ "$expected" != "$actual" ]
	then
		echo "$(tput setaf 1)Wrong output!$(tput sgr0)";
		return 1
	fi

	echo "$(tput setaf 2)Ok!$(tput sgr0)"
	return 0
}

# Default should be a.out in current directory
$lacc foo.c bar.c
a=$(check "a.out"); result="$?"; retval=$((retval + result))

# Compile with position independent code
$lacc -fPIC foo.c bar.c -o foo
b=$(check "foo"); result="$?"; retval=$((retval + result))

# Shared library
$lacc -shared -fPIC foo.c -o libfoo.so
$lacc bar.c -lfoo -L. -o a.out
c=$(check "a.out"); result="$?"; retval=$((retval + result))

echo "[a.out: ${a}] [-fPIC: ${b}] [-shared: ${c}]"
rm -f foo.o bar.o a.cc.out a.out libfoo.so foo
exit $retval
