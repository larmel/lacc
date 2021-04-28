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

INCLUDES = \
	include/stdlib/alloca.h \
	include/stdlib/float.h \
	include/stdlib/stdalign.h \
	include/stdlib/stdarg.h \
	include/stdlib/stdbool.h \
	include/stdlib/stddef.h

LIBDIR_SOURCE = $(SRCDIR)/bin
LIBDIR_TARGET = $(LIBDIR)/lacc
TARGET = bin/selfhost/lacc

bin/lacc: $(SOURCES) bin/include bin/revision.h
	@mkdir -p $(@D)
	$(CC) -std=c89 -g $(CFLAGS) -Iinclude src/lacc.c -o $@ \
		-include bin/revision.h \
		-D'LACC_LIB_PATH="$(LIBDIR_SOURCE)"' \
		-DAMALGAMATION

bin/release/lacc: $(SOURCES) bin/include bin/revision.h
	@mkdir -p $(@D)
	$(CC) -std=c89 -O3 $(CFLAGS) -Iinclude src/lacc.c -o $@ \
		-include bin/revision.h \
		-D'LACC_LIB_PATH="$(LIBDIR_TARGET)"' \
		-DAMALGAMATION \
		-DNDEBUG

bin/bootstrap/lacc: bin/lacc
	@mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		target=$(@D)/$$(basename $$file .c).o ; \
		$? -std=c89 -Iinclude -c $$file -o $$target \
			-D'LACC_LIB_PATH="$(LIBDIR_SOURCE)"' ; \
	done
	$(CC) $(@D)/*.o -o $@

bin/selfhost/lacc: bin/bootstrap/lacc
	@mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		name=$$(basename $$file .c) ; \
		target=$(@D)/$${name}.o ; \
		$? -std=c89 -Iinclude -c $$file -o $$target \
			-D'LACC_LIB_PATH="$(LIBDIR_SOURCE)"' ; \
		diff bin/bootstrap/$${name}.o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

bin/include: $(INCLUDES)
	mkdir -p $@
	cp $? --target-directory=$@

bin/revision.h: $(SOURCES)
	mkdir -p $(@D)
	echo -n '#define LACC_GIT_REVISION "' > $@
	git rev-parse --short HEAD | tr -d "\n" >> $@
	echo '"' >> $@

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

test-asm: $(TARGET)
	for file in $$(find test/asm -type f -iname '*.c') ; do \
		./check.sh "$?" "$$file" "$(CC) -w" ; \
	done

test-extensions: $(TARGET)
	for file in $$(find test/extensions -type f -iname '*.c') ; do \
		./check.sh "$?" "$$file" "$(CC) -w" ; \
	done

test-limits: $(TARGET)
	for file in $$(find test/limits -type f -iname '*.c') ; do \
		$? "$$file" -o "$$file"".out" && "./""$$file"".out" ; \
		rm "$$file"".out" ; \
	done

test-undefined: $(TARGET)
	for file in $$(find test/undefined -type f -iname '*.c') ; do \
		$? "$$file" -o "$$file"".out" && "./""$$file"".out" ; \
		rm "$$file"".out" ; \
	done

test-linker: $(TARGET)
	./linker.sh $?

test-sqlite: $(TARGET)
	./sqlite.sh $? "$(CC)"

test: test-c89 test-c99 test-c11 test-limits
test-all: test test-undefined test-extensions test-asm test-linker test-sqlite

install: bin/release/lacc
	mkdir -p $(LIBDIR_TARGET)/include/ $(BINDIR)
	cp $(LIBDIR_SOURCE)/include/*.h $(LIBDIR_TARGET)/include/
	cp $? $(BINDIR)/lacc

uninstall:
	rm -rf $(LIBDIR_TARGET)
	rm $(BINDIR)/lacc

clean:
	rm -rf bin
	rm -f test/*.out test/*.txt test/*.s

.PHONY: install uninstall clean test test-c89 test-c99 test-c11 test-limits \
	test-undefined test-extensions test-asm \
	test-linker test-sqlite test-all
