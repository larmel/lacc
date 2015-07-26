.PHONY: all bootstrap test test-bootstrap clean

all: bin/lcc
bootstrap: bin/bootstrap

#
# Build the compiler from assembly code built by itself (bootstrapping)
#
bin/bootstrap: bin/lcc bin/abi.o bin/asm.o bin/cfg.o bin/dot.o bin/error.o bin/eval.o bin/input.o bin/lcc.o bin/macro.o bin/parse.o bin/preprocess.o bin/string.o bin/symtab.o bin/tokenize.o bin/type.o bin/libutil.a
	cc -Wall -pedantic -ansi bin/*.o -L./bin/ -lutil -o $@

bin/string.o: bin/string.s
	cc -Wall -c $< -o $@

bin/string.s: src/string.c
	bin/lcc -S -I /usr/include/x86_64-linux-musl/ $< -o $@

bin/%.o: src/%.c
	cc -Wall -pedantic -c $< -o $@

#
# Build the compiler using gcc
#
bin/lcc: src/*.c bin/libutil.a
	cc -Wall -Wpedantic -g $+ -o $@ -L./bin/ -lutil

bin/libutil.a: bin/util/map.o bin/util/stack.o
	ar -cvq $@ $+

bin/util/%.o: src/util/%.c
	cc -Wall -Wpedantic -c $< -o $@

#
# Tests
#
test: bin/lcc
	@for file in test/*.c; do \
		./check.sh $< $$file ; \
	done

test-bootstrap: bin/bootstrap
	@for file in test/*.c; do \
		./check.sh $< $$file ; \
	done

#
# Clean
#
clean:
	rm -rf bin/*
	mkdir -p bin/util
	rm -f test/*.out test/*.s test/*.txt
