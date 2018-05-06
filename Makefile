ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
DIRS := ${shell find src -type d -print}
SOURCES := $(foreach sdir,$(DIRS),$(wildcard $(sdir)/*.c))

INSTALL_PATH := /usr/local

SOURCE_LIB_PATH := $(ROOT)/include/stdlib
INSTALL_LIB_PATH := $(INSTALL_PATH)/lib/lacc/include
INSTALL_BIN_PATH := $(INSTALL_PATH)/bin
CSMITH_INCLUDE_PATH ?= /usr/include/csmith

ifeq ($(origin CC), default)
CC := gcc -Wno-psabi
ifeq ($(shell gcc -v 2>&1 >/dev/null | grep "enable-default-pie" > /dev/null; echo $$?), 0)
CC += -no-pie
endif
endif
CFLAGS ?= -Wall -pedantic -std=c89 -I include/ -Wno-missing-braces
LACCFLAGS := -I include/ -D'LACC_STDLIB_PATH="$(SOURCE_LIB_PATH)"'

all: bin/lacc

bin/lacc: $(SOURCES)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -g -D'LACC_STDLIB_PATH="$(SOURCE_LIB_PATH)"' $^ -o $@

bin/release: $(SOURCES)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -O3 -D'LACC_STDLIB_PATH="$(INSTALL_LIB_PATH)"' -DAMALGAMATION -DNDEBUG src/lacc.c -o $@

bin/bootstrap: $(patsubst src/%.c,bin/%-bootstrap.o,$(SOURCES))
	$(CC) -pie $^ -o $@

bin/%-bootstrap.o: src/%.c bin/lacc
	@mkdir -p $(dir $@)
	bin/lacc -fPIC $(LACCFLAGS) -c $< -o $@

bin/selfhost: $(patsubst src/%.c,bin/%-selfhost.o,$(SOURCES))
	$(CC) -pie $^ -o $@

bin/%-selfhost.o: src/%.c bin/bootstrap
	@mkdir -p $(dir $@)
	bin/bootstrap -fPIC $(LACCFLAGS) -c $< -o $@

test-%: bin/%
	@$(foreach file,$(wildcard test/c11/*.c),\
		./check.sh "$< -std=c11" $(file) "$(CC) -std=c11 -w";)
	@$(foreach file,$(wildcard test/c99/*.c),\
		./check.sh "$< -std=c99" $(file) "$(CC) -std=c99 -w";)
	@$(foreach file,$(wildcard test/*.c),\
		./check.sh "$< -std=c89" $(file) "$(CC) -std=c89 -w";)
	@$(foreach file,$(wildcard test/gnu/*.c),\
		./check.sh "$< -fPIC" $(file) "gcc -std=c89 -w";)

test: test-lacc

install: bin/release
	mkdir -p $(INSTALL_LIB_PATH)
	cp $(SOURCE_LIB_PATH)/*.h $(INSTALL_LIB_PATH)/
	cp $< $(INSTALL_BIN_PATH)/lacc

uninstall:
	rm -rf $(INSTALL_LIB_PATH)
	rm $(INSTALL_BIN_PATH)/lacc

csmith-test: bin/lacc
	@mkdir -p csmith
	./csmith.sh "$(CSMITH_INCLUDE_PATH)" "$(CC)"

creduce-prepare-%: csmith/%.c bin/lacc
	@mkdir -p creduce
	bin/lacc -std=c99 -I $(CSMITH_INCLUDE_PATH) -w -E $< -o creduce/reduce.c
	bin/lacc -std=c99 -c -I $(CSMITH_INCLUDE_PATH) $< -o creduce/reduce.o
	$(CC) creduce/reduce.o -o creduce/reduce -lm
	$(CC) -std=c99 -I $(CSMITH_INCLUDE_PATH) $< -o creduce/reduce-cc
	cp creduce.sh creduce/
	creduce/reduce 1 > creduce/lacc.out && creduce/reduce-cc 1 > creduce/cc.out
	diff --side-by-side --suppress-common-lines creduce/lacc.out creduce/cc.out | head -n 1

creduce-check: bin/lacc
	./check.sh "bin/lacc -std=c99" creduce/reduce.c "$(CC) -std=c99"

sqlite-test: bin/lacc
	./sqlite.sh "bin/lacc -std=c89" "$(CC) -std=c89"

clean:
	rm -rf bin
	rm -f test/*.out test/*.txt test/*.s

.PHONY: all install uninstall test test-% %-test csmith-% creduce-% clean
