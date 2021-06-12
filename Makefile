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
	lib/lacc/include/stddef.h

TARGET = bin/selfhost/lacc

bin/lacc: bin/configure.h $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -Iinclude -include bin/configure.h -DAMALGAMATION src/lacc.c -o $@

bin/bootstrap/lacc: bin/lacc
	mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		target=$(@D)/$$(basename $$file .c).o ; \
		$? -std=c89 -Iinclude -include bin/configure.h -c $$file -o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

bin/selfhost/lacc: bin/bootstrap/lacc
	mkdir -p $(@D)
	for file in $(SOURCES) ; do \
		name=$$(basename $$file .c) ; \
		target=$(@D)/$${name}.o ; \
		$? -std=c89 -Iinclude -include bin/configure.h -c $$file -o $$target ; \
		diff bin/bootstrap/$${name}.o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

bin/configure.h: $(SOURCES) $(HEADERS)
	mkdir -p $(@D)
	echo '#define LACC_LIB_PATH "$(LIBDIR)/lacc"' > $@
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

test: test-c89 test-c99 test-c11 test-limits
test-all: test test-undefined test-extensions test-asm test-linker test-sqlite

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

clean:
	rm -rf bin
	rm -f test/*.out test/*.txt test/*.s

.PHONY: install uninstall clean test test-c89 test-c99 test-c11 test-limits \
	test-undefined test-extensions test-asm \
	test-linker test-sqlite test-all
