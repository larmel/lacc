#!/bin/sh

if test -t 1
then
    colors=$(tput colors)
    if test -n "$colors" && test $colors -ge 8
    then
        reset="$(tput sgr0)"
        red="$(tput setaf 1)"
        green="$(tput setaf 2)"
    fi
fi

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
		echo "${red}Compilation failed!${reset}";
		return 1
	fi

	if [ ! -f "$1" ]
	then
		echo "${red}Did not create $1!${reset}";
		return 1
	fi

	actual=$(LD_LIBRARY_PATH=. ./$1 1 2 3)

	if [ "$expected" != "$actual" ]
	then
		echo "${red}Wrong output!${reset}";
		return 1
	fi

	echo "${green}Ok!${reset}"
	return 0
}

# Default should be a.out in current directory
$lacc -fno-PIC foo.c bar.c
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
