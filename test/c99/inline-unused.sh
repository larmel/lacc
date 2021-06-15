#!/bin/sh

cc=$1
src=$2
dir=$3
$cc -c ${src}.c -o ${dir}/${src}.o || exit 1

readelf -s ${dir}/${src}.o | awk '
	BEGIN { wrong=0; }
	$8 ~ /(foo|bar|puts)/ {
		print "Unexpected symbol", $8;
		wrong += 1;
	}
	END { exit wrong }';

exit $?
