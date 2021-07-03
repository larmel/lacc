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
	lacc=../bin/lacc
	command -v $lacc >/dev/null 2>&1 || {
		echo "$lacc required, run 'make'."
		exit 1
	}
fi

# Default should be a.out in current directory
rm -f a.out
$lacc linker/foo.c linker/bar.c
if [ $? -ne 0 ] || [ ! -e a.out ]
then
	echo "${red}Failed to produce default output a.out!${reset}";
	return 1
fi
rm a.out

bin=../bin/test/linker
mkdir -p $bin

# Target is always the same
cc linker/foo.c linker/bar.c -o $bin/a.out
expected=$($bin/a.out 1 2 3)

check()
{
	if [ $? -ne 0 ]
	then
		echo "${red}Compilation failed!${reset}";
		return 1
	fi

	if [ ! -f "$bin/$1" ]
	then
		echo "${red}Did not create $1!${reset}";
		return 1
	fi

	actual=$(LD_LIBRARY_PATH=$bin $bin/$1 1 2 3)

	if [ "$expected" != "$actual" ]
	then
		echo "${red}Wrong output!${reset}";
		return 1
	fi

	echo "${green}Ok!${reset}"
	return 0
}

$lacc -fno-PIC linker/foo.c linker/bar.c -o $bin/a.out
a=$(check "a.out"); result="$?"; retval=$((retval + result))

$lacc -fPIC linker/foo.c linker/bar.c -o $bin/foo
b=$(check "foo"); result="$?"; retval=$((retval + result))

# Shared library
$lacc -shared -fPIC linker/foo.c -o $bin/libfoo.so
$lacc linker/bar.c -lfoo -L$bin -o $bin/a.out
c=$(check "a.out"); result="$?"; retval=$((retval + result))

echo "[-fno-PIC: ${a}] [-fPIC: ${b}] [-shared: ${c}]"
rm -f foo.o bar.o
exit $retval
