all: bin/lcc

bin/lcc: src/*.c src/*.h
	cc -Wall -Wpedantic $+ -o $@
