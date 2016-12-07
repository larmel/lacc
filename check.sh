#!/bin/bash

prog="$1"
file="$2"
comp="$3"
if [[ -z "$file" || ! -f "$file" ]]; then
	echo "Usage: $0 <compiler> <file> {<reference compiler>}";
	exit 1
fi

if [[ -z "$comp" ]]; then
	comp="cc -std=c89 -Wno-psabi"
fi

$comp $file -o ${file}.out
if [ "$?" -ne "0" ]; then
	echo "${file}: $(tput setaf 1)Invalid input file!$(tput sgr 0)";
	exit 1
fi
./${file}.out > ${file}.ans.txt; answer="$?"

function check {
	$prog $1 $file -o ${file}.s
	if [ "$?" -ne "0" ]; then
		echo "$(tput setaf 1)Compilation failed!$(tput sgr 0)";
		return 1
	fi
	if [ "$1" == "-E" ]; then
		mv ${file}.s ${file}.prep.c
		$comp -c ${file}.prep.c -o ${file}.o
		if [ "$?" -ne "0" ]; then
			echo "$(tput setaf 1)Compilation failed!$(tput sgr 0)";
			return 1
		fi
	elif [ "$1" == "-S" ]; then
		cc -c ${file}.s -o ${file}.o
		if [ "$?" -ne "0" ]; then
			echo "$(tput setaf 1)Assembly failed!$(tput sgr 0)";
			return 1
		fi
	else
		mv ${file}.s ${file}.o
	fi
	cc ${file}.o -o ${file}.out -lm
	if [ "$?" -ne "0" ]; then
		echo "$(tput setaf 1)Linking failed!$(tput sgr 0)";
		return 1
	fi

	./${file}.out > ${file}.txt
	result="$?"
	difference=`diff ${file}.ans.txt ${file}.txt`
	diffres="$?"

	if [[ "$result" -eq "$answer" && "$diffres" -eq "0" ]]; then
		echo "$(tput setaf 2)Ok!$(tput sgr 0)"
		return 0
	else
		echo "$(tput setaf 1)Wrong result!$(tput sgr 0)"
		if [ "$result" -ne "$answer" ]; then
			echo "Result differ: was ${result}, expected ${answer}." >&2
		fi
		if [ "$diffres" -ne "0" ]; then
			echo "Output differ:"  >&2
			echo "$difference"  >&2
		fi
		return 1
	fi
}

prp=$(check -E); result="$?"; retval=$((retval + result))
asm=$(check -S); result="$?"; retval=$((retval + result))
elf=$(check -c); result="$?"; retval=$((retval + result))
opt=$(check "-c -O1"); result="$?"; retval=$((retval + result))

echo "[-E: ${prp}] [-S: ${asm}] [-c: ${elf}] [-c -O1: ${opt}] :: ${file}"
rm -f ${file}.out ${file}.ans.txt ${file}.txt ${file}.s ${file}.prep.c ${file}.o

exit $retval
