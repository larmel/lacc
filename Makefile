.POSIX:
.SUFFIXES:
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
LIBDIR = $(PREFIX)/lib

CC = cc
CFLAGS = -std=c89 -g -Wall -pedantic -Wno-missing-braces

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

HEADERS = \
	include/lacc/array.h \
	include/lacc/context.h \
	include/lacc/deque.h \
	include/lacc/hash.h \
	include/lacc/ir.h \
	include/lacc/string.h \
	include/lacc/symbol.h \
	include/lacc/token.h \
	include/lacc/type.h \
	src/util/argparse.h \
	src/backend/x86_64/encoding.h \
	src/backend/x86_64/dwarf.h \
	src/backend/x86_64/elf.h \
	src/backend/x86_64/abi.h \
	src/backend/x86_64/assemble.h \
	src/backend/assembler.h \
	src/backend/compile.h \
	src/backend/graphviz/dot.h \
	src/backend/linker.h \
	src/optimizer/transform.h \
	src/optimizer/liveness.h \
	src/optimizer/optimize.h \
	src/preprocessor/tokenize.h \
	src/preprocessor/strtab.h \
	src/preprocessor/input.h \
	src/preprocessor/directive.h \
	src/preprocessor/preprocess.h \
	src/preprocessor/macro.h \
	src/parser/typetree.h \
	src/parser/symtab.h \
	src/parser/parse.h \
	src/parser/statement.h \
	src/parser/initializer.h \
	src/parser/expression.h \
	src/parser/declaration.h \
	src/parser/eval.h

LIBS = \
	lib/lacc/include/alloca.h \
	lib/lacc/include/float.h \
	lib/lacc/include/stdalign.h \
	lib/lacc/include/stdarg.h \
	lib/lacc/include/stdbool.h \
	lib/lacc/include/stddef.h \
	lib/lacc/include/stdnoreturn.h

TARGET = bin/selfhost/lacc

bin/lacc: bin/config.h $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -Iinclude -include bin/config.h -DAMALGAMATION src/lacc.c -o $@

bin/bootstrap/lacc: bin/lacc
	mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		target=$(@D)/$$(basename $$file .c).o ; \
		$? -std=c89 -Iinclude -include bin/config.h -c $$file -o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

bin/selfhost/lacc: bin/bootstrap/lacc
	mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		name=$$(basename $$file .c) ; \
		target=$(@D)/$${name}.o ; \
		$? -std=c89 -Iinclude -include bin/config.h -c $$file -o $$target ; \
		diff bin/bootstrap/$${name}.o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

bin/config.h: $(SOURCES) $(HEADERS)
	mkdir -p $(@D)
	if ldd /bin/ls | grep "x86_64-linux-gnu" > /dev/null; then \
		echo '#define GLIBC 1' > $@ ; \
		echo '#define SYSTEM_LIB_PATH "/usr/include/x86_64-linux-gnu"' >> $@ ; \
	elif ldd /bin/ls | grep "musl" > /dev/null ; then \
		echo '#define MUSL 1' > $@ ; \
	fi
	echo '#define LACC_LIB_PATH "$(LIBDIR)/lacc"' >> $@
	echo -n '#define LACC_GIT_REVISION "' >> $@
	git rev-parse --short HEAD | tr -d "\n" >> $@
	echo '"' >> $@

install: $(LIBS)
	mkdir -p $(LIBDIR)/lacc/include $(BINDIR)
	cp $(LIBS) $(LIBDIR)/lacc/include
	cp bin/lacc $(BINDIR)/lacc

uninstall:
	rm -rf $(LIBDIR)/lacc
	rm $(BINDIR)/lacc

clean:
	rm -rf bin

test/c89 test/c99 test/c11: $(TARGET)
	mkdir -p bin/$@
	std=$$(echo $@ | cut -d '/' -f 2) ; \
	for file in $$(find $@ -maxdepth 1 -type f -iname '*.c') ; do \
		test/check.sh "$? -std=$$std" $$file "$(CC) -std=$$std -w" bin ; \
	done

test/asm test/extensions: $(TARGET)
	mkdir -p bin/$@
	for file in $$(find $@ -type f -iname '*.c') ; do \
		test/check.sh $? $$file "$(CC) -w" bin ; \
	done

test/limits test/undefined: $(TARGET)
	mkdir -p bin/$@
	for file in $$(find $@ -type f -iname '*.c') ; do \
		$? $$file -c -o bin/$$file.o ; \
		$? bin/$$file.o -o bin/$$file.out && bin/$$file.out ; \
	done

test: test/c89 test/c99 test/c11 test/limits test/undefined

test-all: $(TARGET) test test/extensions test/asm
	test/linker.sh $(TARGET)
	test/sqlite.sh $(TARGET)
	test/csmith.sh

.PHONY: install uninstall clean \
	test test-all test/c89 test/c99 test/c11 \
	test/asm test/extensions test/limits test/undefined
