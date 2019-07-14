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
file="$2"
comp="$3"
if [ -z "$file" ] || [ ! -f "$file" ]; then
	echo "Usage: $0 <compiler> <file> [<reference compiler>]";
	exit 1
fi

if [ -z "$comp" ]; then
	comp="gcc -std=c89 -Wno-psabi"
fi

$comp -v 2>&1 >/dev/null | grep "enable-default-pie" > /dev/null
if [ "$?" -eq "0" ]; then
	lacc="${lacc} -fPIC"
fi

$comp $file -o ${file}.out
if [ "$?" -ne "0" ]; then
	echo "${file}: ${red}Invalid input file!${reset}";
	exit 1
fi

./${file}.out > ${file}.ans.txt; answer="$?"

check()
{
	$lacc $1 $file -o ${file}.s
	if [ "$?" -ne "0" ]; then
		echo "${red}Compilation failed!${reset}";
		return 1
	fi
	if [ "$1" = "-E" ]; then
		mv ${file}.s ${file}.i
		$lacc -c ${file}.i -o ${file}.o
		if [ "$?" -ne "0" ]; then
			echo "${red}Compilation failed!${reset}";
			return 1
		fi
	elif [ "$1" = "-S" ]; then
		$comp -c ${file}.s -o ${file}.o
		if [ "$?" -ne "0" ]; then
			echo "${red}Assembly failed!${reset}";
			return 1
		fi
	else
		mv ${file}.s ${file}.o
	fi
	$comp ${file}.o -o ${file}.out -lm
	if [ "$?" -ne "0" ]; then
		echo "${red}Linking failed!${reset}";
		return 1
	fi

	./${file}.out > ${file}.txt
	result="$?"
	difference=`diff ${file}.ans.txt ${file}.txt`
	diffres="$?"

	if [ "$result" -eq "$answer" ] && [ "$diffres" -eq "0" ]; then
		echo "${green}Ok!${reset}"
		return 0
	else
		echo "${red}Wrong result!${reset}"
		if [ "$result" -ne "$answer" ]; then
			echo "Result differ: was ${result}, expected ${answer}." >&2
		fi
		if [ "$diffres" -ne "0" ]; then
			echo "Output differ:" >&2
			echo "$difference" >&2
		fi
		return 1
	fi
}

prp=$(check -E); result="$?"; retval=$((retval + result))
asm=$(check -S); result="$?"; retval=$((retval + result))
elf=$(check -c); result="$?"; retval=$((retval + result))
opt=$(check "-c -O1"); result="$?"; retval=$((retval + result))
echo "[-E: ${prp}] [-S: ${asm}] [-c: ${elf}] [-c -O1: ${opt}] :: ${file}"

if [ $retval -eq 0 ] && [ -f "${file}.sh" ]
then
	./${file}.sh "$lacc" "$file"
	retval=$?
	if [ $retval -ne 0 ]
	then
		echo "${red}Failed validation script!${reset}"
	fi
fi

rm -f ${file}.out ${file}.ans.txt ${file}.txt ${file}.s ${file}.i ${file}.o
exit $retval
