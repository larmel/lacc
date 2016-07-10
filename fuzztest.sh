#!/bin/bash

csmith_home="$1"
csmith_options="--no-packed-struct"
if [[ -z "$csmith_home" ]]; then
	echo "Usage: $0 <csmith home path>"; exit
fi

n=1
while [ true ]
do
	filename="fuzz/${n}.c"
	${csmith_home}/src/csmith ${csmith_options} > ${filename};
	./check.sh \
		"bin/lacc -w -std=c99 -I ${csmith_home}/runtime" ${filename} \
		"cc -w -std=c99 -I ${csmith_home}/runtime";
	result="$?"
	if [[ $result -ne "0" ]]; then
		break
	fi
	n=$((n + 1))
done
