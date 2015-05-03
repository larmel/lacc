#!/bin/bash

file="$1"
if [[ -z "$file" || ! -f "$file" ]]; then
	echo "Usage: $0 file"
	exit
fi

# Compile with gcc
gcc $file -o ${file}.out
if [ "$?" -ne "0" ]; then
	echo "${file}: $(tput setaf 1)Invalid input file!$(tput sgr 0)"
	exit
fi
./${file}.out > ${file}.expected.txt
answer="$?"

# Compile with lcc, and assemble with gcc
bin/lcc -S -I /usr/include/x86_64-linux-musl/ $file -o ${file}.out.s
if [ "$?" -ne "0" ]; then
	echo "${file}: $(tput setaf 1)Compilation failed!$(tput sgr 0)"
	exit
fi
gcc ${file}.out.s -o ${file}.out
if [ "$?" -ne "0" ]; then
	echo "${file}: $(tput setaf 1)Assembly failed!$(tput sgr 0)"
	exit
fi
./${file}.out > ${file}.txt
result="$?"

# Compare result and output diff
difference=`diff ${file}.expected.txt ${file}.txt`
diffres="$?"
if [[ "$result" -eq "$answer" && "$diffres" -eq "0" ]]; then
	echo "${file}: $(tput setaf 2)Success!$(tput sgr 0)"
	rm -f ${file}.out ${file}.expected.txt ${file}.out.s ${file}.txt
else
	echo "${file}: $(tput setaf 1)Wrong result!$(tput sgr 0)"
	if [ "$result" -ne "$answer" ]; then
		echo "Result differ: was ${result}, expected ${answer}."
	fi
	if [ "$diffres" -ne "0" ]; then
		echo "Output differ:"
		echo "$difference"
	fi
fi
