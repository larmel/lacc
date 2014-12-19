.PHONY: all clean

all: bin/lcc

bin/lcc: src/*.c bin/libutil.a
	cc -Wall -Wpedantic $+ -o $@ -L./bin/ -lutil

bin/libutil.a: bin/util/map.o
	ar -cvq $@ $+

bin/util/%.o: src/util/%.c
	mkdir -p bin/util
	cc -Wall -Wpedantic -c $< -o $@

clean:
	rm -rf bin/*
