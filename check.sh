#!/bin/bash

prog="$1"
file="$2"
if [[ -z "$file" || ! -f "$file" ]]; then
	echo "Usage: $0 <compiler> <file>"; exit
fi

cc $file -o ${file}.out
if [ "$?" -ne "0" ]; then
	echo "${file}: $(tput setaf 1)Invalid input file!$(tput sgr 0)"; exit
fi
./${file}.out > ${file}.ans.txt
answer="$?"

function check {
	$prog $1 $file -o ${file}.s
	if [ "$?" -ne "0" ]; then
		echo "$(tput setaf 1)Compilation failed!$(tput sgr 0)"; return
	fi
	if [ "$1" == "-E" ]; then
		mv ${file}.s ${file}.prep.c
		cc -c ${file}.prep.c -o ${file}.o
		if [ "$?" -ne "0" ]; then
			echo "$(tput setaf 1)Compilation failed!$(tput sgr 0)"; return
		fi
	elif [ "$1" == "-S" ]; then
		cc -c ${file}.s -o ${file}.o
		if [ "$?" -ne "0" ]; then
			echo "$(tput setaf 1)Assembly failed!$(tput sgr 0)"; return
		fi
	else
		mv ${file}.s ${file}.o
	fi
	cc ${file}.o -o ${file}.out
	if [ "$?" -ne "0" ]; then
		echo "$(tput setaf 1)Linking failed!$(tput sgr 0)"; return
	fi

	./${file}.out > ${file}.txt
	result="$?"
	difference=`diff ${file}.ans.txt ${file}.txt`
	diffres="$?"

	if [[ "$result" -eq "$answer" && "$diffres" -eq "0" ]]; then
		echo "$(tput setaf 2)Success!$(tput sgr 0)"
	else
		echo "$(tput setaf 1)Wrong result!$(tput sgr 0)"
		if [ "$result" -ne "$answer" ]; then
			echo "Result differ: was ${result}, expected ${answer}." >&2
		fi
		if [ "$diffres" -ne "0" ]; then
			echo "Output differ:"  >&2
			echo "$difference"  >&2
		fi
	fi
}

echo "[-E: $(check "-E")] [-S: $(check "-S")] [-c: $(check "-c")] :: ${file}"
rm -f ${file}.out ${file}.ans.txt ${file}.txt ${file}.s ${file}.prep.c ${file}.o
