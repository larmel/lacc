#!/bin/bash

for file in test/*.c; do
	cc $file -o ${file}.out
	${file}.out > ${file}.expected.txt
	answer="$?"

	bin/lcc -S $file -o ${file}.out.s
	result="$?"
	if [ "$result" -ne "0" ]; then
		echo "${file}: $(tput setaf 1)Compilation failed!$(tput sgr 0)"
		continue
	fi

	gcc ${file}.out.s -o ${file}.out
	result="$?"
	if [ "$result" -ne "0" ]; then
		echo "${file}: $(tput setaf 1)Assembly failed!$(tput sgr 0)"
		continue
	fi
	${file}.out > ${file}.txt
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
done
