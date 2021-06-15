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
	lacc=bin/lacc
	command -v bin/lacc >/dev/null 2>&1 || {
		echo "bin/lacc required, run 'make'."
		exit 1
	}
fi

# Default should be a.out in current directory
rm -f a.out
$lacc test/linker/foo.c test/linker/bar.c
if [ $? -ne 0 ] || [ ! -e a.out ]
then
	echo "${red}Failed to produce default output a.out!${reset}";
	return 1
fi
rm a.out

mkdir -p bin/test/linker

# Target is always the same
cc test/linker/foo.c test/linker/bar.c -o bin/test/linker/a.out
expected=$(bin/test/linker/a.out 1 2 3)

check()
{
	if [ $? -ne 0 ]
	then
		echo "${red}Compilation failed!${reset}";
		return 1
	fi

	if [ ! -f "bin/test/linker/$1" ]
	then
		echo "${red}Did not create $1!${reset}";
		return 1
	fi

	actual=$(LD_LIBRARY_PATH=bin/test/linker bin/test/linker/$1 1 2 3)

	if [ "$expected" != "$actual" ]
	then
		echo "${red}Wrong output!${reset}";
		return 1
	fi

	echo "${green}Ok!${reset}"
	return 0
}

$lacc -fno-PIC test/linker/foo.c test/linker/bar.c -o bin/test/linker/a.out
a=$(check "a.out"); result="$?"; retval=$((retval + result))

$lacc -fPIC test/linker/foo.c test/linker/bar.c -o bin/test/linker/foo
b=$(check "foo"); result="$?"; retval=$((retval + result))

# Shared library
$lacc -shared -fPIC test/linker/foo.c -o bin/test/linker/libfoo.so
$lacc test/linker/bar.c -lfoo -Lbin/test/linker -o bin/test/linker/a.out
c=$(check "a.out"); result="$?"; retval=$((retval + result))

echo "[-fno-PIC: ${a}] [-fPIC: ${b}] [-shared: ${c}]"
rm -f foo.o bar.o
exit $retval
