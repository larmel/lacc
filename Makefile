all: bin/lcc

bin/lcc: src/*.c
	cc -Wall -Wpedantic $+ -o $@
