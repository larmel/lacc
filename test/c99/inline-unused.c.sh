#!/bin/sh

cc=$1
file=$2
$cc -c $file -o ${file}.o || exit 1

readelf -s ${file}.o | awk '
	BEGIN { wrong=0; }
	$8 ~ /(foo|bar|puts)/ {
		print "Unexpected symbol", $8;
		wrong += 1;
	}
	END { exit wrong }';

exit $?
