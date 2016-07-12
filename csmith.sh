#!/bin/bash

directory="csmith"
csmith_home="$1"
csmith_options="--no-packed-struct --max-funcs 1 --max-struct-fields 2 --max-union-fields 2 --max-block-depth 2 --no-jumps"
if [[ -z "$csmith_home" ]]; then
	echo "Usage: $0 <csmith home path>"; exit
fi

n=1
while [ true ]
do
	filename="${directory}/${n}.c"
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
