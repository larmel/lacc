#!/bin/sh

cc=$1
src=$2
dir=$3
$cc -c ${src}.c -o ${dir}/${src}.o || exit 1

readelf -s ${dir}/${src}.o | awk '
	BEGIN { missing=4; }
	$8 ~ /(foo|bar|baz|puts)/ { missing -= 1; }
	END { exit missing }';

exit $?
