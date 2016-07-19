#!/bin/bash

directory="csmith"
csmith_home="$1"
csmith_options="--no-packed-struct"
if [[ -z "$csmith_home" ]]
then
	echo "Usage: $0 <csmith home path>"
	exit
fi

n=1
while [ true ]
do
	filename="${directory}/${n}.c"
	program="${directory}/${n}"
	${csmith_home}/src/csmith ${csmith_options} > ${filename}

	cc -w -std=c99 -I ${csmith_home}/runtime ${filename} -o ${program}
	timeout 1 ${program} > /dev/null
	if [ $? -ne 0 ]
	then
		rm ${program}
		continue
	fi

	rm ${program}
	./check.sh \
		"bin/lacc -w -std=c99 -I ${csmith_home}/runtime" ${filename} \
		"cc -w -std=c99 -I ${csmith_home}/runtime"
	if [ $? -ne 0 ]
	then
		break
	fi

	n=$((n + 1))
done
