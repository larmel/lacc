.POSIX:
.SUFFIXES:

CC = cc
TARGET = ../bin/selfhost/lacc
BIN = ../bin/test

all: $(TARGET) c89 c99 c11 limits undefined extensions asm linker

extra: sqlite csmith

../bin/bootstrap/lacc: ../bin/lacc
	mkdir -p $(@D)
	for file in $$(find ../src -type f -iname '*.c') ; do \
		target=$(@D)/$$(basename $$file .c).o ; \
		$? -std=c89 -I../include -include ../config.h -c $$file -o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

../bin/selfhost/lacc: ../bin/bootstrap/lacc
	mkdir -p $(@D)
	for file in $$(find ../src -type f -iname '*.c') ; do \
		name=$$(basename $$file .c) ; \
		target=$(@D)/$${name}.o ; \
		$? -std=c89 -I../include -include ../config.h -c $$file -o $$target ; \
		diff ../bin/bootstrap/$${name}.o $$target ; \
	done
	$(CC) $(@D)/*.o -o $@

c89 c99 c11: $(TARGET)
	mkdir -p $(BIN)/$@
	for file in $$(find $@ -maxdepth 1 -type f -iname '*.c') ; do \
		./check.sh "$? -std=$@" $$file "$(CC) -std=$@ -w" $(BIN) ; \
	done

asm extensions: $(TARGET)
	mkdir -p $(BIN)/$@
	for file in $$(find $@ -type f -iname '*.c') ; do \
		./check.sh $? $$file "$(CC) -w" $(BIN) ; \
	done

limits undefined: $(TARGET)
	mkdir -p $(BIN)/$@
	for file in $$(find $@ -type f -iname '*.c') ; do \
		$? $$file -c -o $(BIN)/$$file.o ; \
		$? $(BIN)/$$file.o -o $(BIN)/$$file.out && $(BIN)/$$file.out ; \
	done

linker: $(TARGET)
	./linker.sh $?

sqlite: $(TARGET)
	./sqlite.sh $?

csmith:
	./csmith.sh

.PHONY: all extra c89 c99 c11 asm extensions limits undefined \
	linker sqlite csmith
