.POSIX:
.SUFFIXES:

include config.mak

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
	src/parser/builtin.c \
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
	src/parser/builtin.h \
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

bin/lacc: $(SOURCES) $(HEADERS) config.h config.mak
	mkdir -p $(@D)
	$(CC) $(CPPFLAGS) $(CFLAGS) -Iinclude -include config.h -DAMALGAMATION src/lacc.c -o $@

install:
	install -d -m 755 $(DESTDIR)$(BINDIR)
	install -d -m 755 $(DESTDIR)$(LIBDIR)/lacc/include
	install -m 755 bin/lacc $(DESTDIR)$(BINDIR)
	install -m 644 $(LIBS) $(DESTDIR)$(LIBDIR)/lacc/include

uninstall:
	rm -rf $(DESTDIR)$(LIBDIR)/lacc
	rm -f $(DESTDIR)$(BINDIR)/lacc

clean:
	rm -rf bin

distclean: clean
	rm -f config.h config.mak

.PHONY: install uninstall clean distclean
