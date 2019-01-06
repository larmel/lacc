.POSIX:
.SUFFIXES:
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib
SRCDIR = $(CURDIR)$(.CURDIR)

CC = cc -Wno-psabi
CFLAGS = -Wall -pedantic -Wno-missing-braces

SOURCES = \
	src/lacc.c \
	src/context.c \
	src/util/argparse.c \
	src/util/hash.c \
	src/util/string.c \
	src/backend/x86_64/encoding.c \
	src/backend/x86_64/dwarf.c \
	src/backend/x86_64/elf.c \
	src/backend/x86_64/abi.c \
	src/backend/x86_64/assemble.c \
	src/backend/assembler.c \
	src/backend/compile.c \
	src/backend/graphviz/dot.c \
	src/backend/linker.c \
	src/optimizer/transform.c \
	src/optimizer/liveness.c \
	src/optimizer/optimize.c \
	src/preprocessor/tokenize.c \
	src/preprocessor/strtab.c \
	src/preprocessor/input.c \
	src/preprocessor/directive.c \
	src/preprocessor/preprocess.c \
	src/preprocessor/macro.c \
	src/parser/typetree.c \
	src/parser/symtab.c \
	src/parser/parse.c \
	src/parser/statement.c \
	src/parser/initializer.c \
	src/parser/expression.c \
	src/parser/declaration.c \
	src/parser/eval.c

LIBDIR_SOURCE = $(SRCDIR)/include/stdlib
LIBDIR_TARGET = $(LIBDIR)/lacc/include
TARGET = bin/selfhost/lacc

bin/lacc: $(SOURCES)
	@mkdir -p $(@D)
	$(CC) -std=c89 -g $(CFLAGS) -Iinclude src/lacc.c -o $@ \
		-D'LACC_STDLIB_PATH="$(LIBDIR_SOURCE)"' \
		-DAMALGAMATION

bin/release/lacc: $(SOURCES)
	@mkdir -p $(@D)
	$(CC) -std=c89 -O3 $(CFLAGS) -Iinclude src/lacc.c -o $@ \
		-D'LACC_STDLIB_PATH="$(LIBDIR_TARGET)"' \
		-DAMALGAMATION \
		-DNDEBUG

bin/bootstrap/lacc: bin/lacc
	@mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		target=$(@D)/$$(basename $$file .c).o ; \
		$? -std=c89 -fPIC -Iinclude -c $$file -o $$target \
			-D'LACC_STDLIB_PATH="$(LIBDIR_SOURCE)"' ; \
	done
	$(CC) $(@D)/*.o -o $@

bin/selfhost/lacc: bin/bootstrap/lacc
	@mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		name=$$(basename $$file .c) ; \
		target=$(@D)/$${name}.o ; \
		$? -std=c89 -fPIC -Iinclude -c $$file -o $$target \
			-D'LACC_STDLIB_PATH="$(LIBDIR_SOURCE)"' ; \
		diff bin/bootstrap/$${name}.o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

test-c89: $(TARGET)
	for file in $$(find test/ -maxdepth 1 -type f -iname '*.c') ; do \
		./check.sh "$? -std=c89" "$$file" "$(CC) -std=c89 -w" ; \
	done

test-c99: $(TARGET)
	for file in $$(find test/c99 -type f -iname '*.c') ; do \
		./check.sh "$? -std=c99" "$$file" "$(CC) -std=c99 -w" ; \
	done

test-c11: $(TARGET)
	for file in $$(find test/c11 -type f -iname '*.c') ; do \
		./check.sh "$? -std=c11" "$$file" "$(CC) -std=c11 -w" ; \
	done

test-gnu: $(TARGET)
	for file in $$(find test/gnu -type f -iname '*.c') ; do \
		./check.sh "$?" "$$file" "gcc -w" ; \
	done

test-asm: $(TARGET)
	for file in $$(find test/asm -type f -iname '*.c') ; do \
		./check.sh "$?" "$$file" "$(CC) -w" ; \
	done

test-sqlite: $(TARGET)
	./sqlite.sh $? "$(CC)"

test: test-c89 test-c99 test-c11

install: bin/release/lacc
	mkdir -p $(LIBDIR_TARGET)
	cp $(LIBDIR_SOURCE)/*.h $(LIBDIR_TARGET)/
	cp $? $(BINDIR)/lacc

uninstall:
	rm -rf $(LIBDIR_TARGET)
	rm $(BINDIR)/lacc

clean:
	rm -rf bin
	rm -f test/*.out test/*.txt test/*.s

.PHONY: install uninstall clean test \
	test-c89 test-c99 test-c11 test-gnu test-sqlite
