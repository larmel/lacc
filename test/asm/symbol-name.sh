#!/bin/sh

cc=$1
src=$2
dir=$3
$cc -S ${src}.c -o ${dir}/${src}.s || exit 1

grep _A: ${dir}/${src}.s > /dev/null || exit 1
grep _C: ${dir}/${src}.s > /dev/null || exit 1
grep __D: ${dir}/${src}.s > /dev/null || exit 1
grep BAR: ${dir}/${src}.s > /dev/null || exit 1
grep __foo: ${dir}/${src}.s > /dev/null || exit 1

exit 0
