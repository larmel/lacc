all: bin/lcc

bin/lcc: src/*.c
	cc $+ -o $@
