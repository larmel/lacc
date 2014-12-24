.PHONY: all test clean

all: bin/lcc

bin/lcc: src/*.c bin/libutil.a
	cc -Wall -Wpedantic $+ -o $@ -L./bin/ -lutil

bin/libutil.a: bin/util/map.o bin/util/stack.o
	ar -cvq $@ $+

bin/util/%.o: src/util/%.c
	mkdir -p bin/util
	cc -Wall -Wpedantic -c $< -o $@

test: bin/lcc
	./testrunner.sh

clean:
	rm -rf bin/*
	rm -f test/*.out test/*.s test/*.txt
