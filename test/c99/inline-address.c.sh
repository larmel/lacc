#!/bin/sh

cc=$1
file=$2
$cc -c $file -o ${file}.o || exit 1

readelf -s ${file}.o | awk '
	BEGIN { missing=4; }
	$8 ~ /(foo|bar|baz|puts)/ { missing -= 1; }
	END { exit missing }';

exit $?
