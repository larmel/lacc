.PHONY: all test clean

all: bin/lcc

self: bin/ccl

bin/ccl: bin/asm.o bin/cfg.o bin/dot.o bin/error.o bin/eval.o bin/input.o bin/lcc.o bin/macro.o bin/parse.o bin/preprocess.o bin/string.o bin/symtab.o bin/tokenize.o bin/type.o bin/var.o bin/libutil.a
	cc -Wall -pedantic -ansi bin/*.o -L./bin/ -lutil -o $@

# Subset of files that can (not) be compiled with lcc
bin/input.o: bin/input.s
	cc -Wall -pedantic -ansi -c $< -o $@

bin/input.s: src/input.c
	bin/lcc -S -I /usr/include/x86_64-linux-musl/ $< -o $@

bin/%.o: src/%.c
	cc -Wall -pedantic -c $< -o $@

# Build lcc with gcc
bin/lcc: src/*.c bin/libutil.a
	cc -Wall -Wpedantic -g $+ -o $@ -L./bin/ -lutil

bin/libutil.a: bin/util/map.o bin/util/stack.o
	ar -cvq $@ $+

bin/util/%.o: src/util/%.c
	mkdir -p bin/util
	cc -Wall -Wpedantic -c $< -o $@

test: bin/lcc
	@for file in test/*.c; do \
		./check.sh $$file ; \
	done

clean:
	rm -rf bin/*
	rm -f test/*.out test/*.s test/*.txt
