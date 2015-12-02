BIN := bin
SRC_ROOT := src
SRC_DIRS := ${shell find ${SRC_ROOT} -type d -print}
TESTS := $(wildcard test/*.c)

LD := cc
CC := cc
CCFLAGS := -Wall -pedantic -std=c89 -g -I include/
LACCFLAGS := -I /usr/include/x86_64-linux-musl/ -I include/

# Normal build with gcc
SOURCES := $(foreach sdir,$(SRC_DIRS),$(wildcard $(sdir)/*.c))
OBJECTS := $(patsubst src/%.c,$(BIN)/%.o,$(SOURCES))

# Bootstrap build subset of files
BOOTSTRAP_SOURCES := \
	src/backend/graphviz/dot.c \
	src/backend/x86_64/abi.c \
	src/backend/x86_64/assemble.c \
	src/backend/compile.c \
	src/parser/cfg.c \
	src/parser/eval.c \
	src/parser/parse.c \
	src/parser/string.c \
	src/parser/symtab.c \
	src/parser/type.c \
	src/preprocessor/input.c \
	src/preprocessor/macro.c \
	src/preprocessor/preprocess.c \
	src/preprocessor/tokenize.c \
	src/cli.c \
	src/main.c
BOOTSTRAP_OBJECTS := $(patsubst src/%.c,$(BIN)/%-bootstrap.o,$(BOOTSTRAP_SOURCES))
REMAINING_SOURCES := $(filter-out $(BOOTSTRAP_SOURCES), $(SOURCES))
REMAINING_OBJECTS := $(patsubst src/%.c,$(BIN)/%.o,$(REMAINING_SOURCES))

# Selfhosted, compiler built with itself
SELFHOST_OBJECTS := $(patsubst src/%.c,$(BIN)/%-selfhost.o,$(SOURCES))

.PHONY: all bootstrap selfhost test test-bootstrap test-selfhost clean

all: $(BIN)/lacc
bootstrap: $(BIN)/bootstrap
selfhost: $(BIN)/selfhost

$(BIN)/%.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CCFLAGS) -c $< -o $@

$(BIN)/%-bootstrap.o: $(BIN)/%-bootstrap.s
	@mkdir -p $(dir $@)
	$(CC) -c $< -o $@

$(BIN)/%-bootstrap.s: src/%.c $(BIN)/lacc
	@mkdir -p $(dir $@)
	$(BIN)/lacc $(LACCFLAGS) -S $< -o $@

$(BIN)/%-selfhost.o: $(BIN)/%-selfhost.s
	@mkdir -p $(dir $@)
	$(CC) -c $< -o $@

$(BIN)/%-selfhost.s: src/%.c $(BIN)/bootstrap
	@mkdir -p $(dir $@)
	$(BIN)/bootstrap $(LACCFLAGS) -S $< -o $@

$(BIN)/lacc: $(OBJECTS)
	$(LD) $^ -o $@

$(BIN)/bootstrap: $(BOOTSTRAP_OBJECTS) $(REMAINING_OBJECTS)
	$(LD) $^ -o $@

$(BIN)/selfhost: $(SELFHOST_OBJECTS)
	$(LD) $^ -o $@

test: $(BIN)/lacc
	@$(foreach file,$(TESTS),./check.sh $< $(file);)

test-bootstrap: $(BIN)/bootstrap
	@$(foreach file,$(TESTS),./check.sh $< $(file);)

test-selfhost: $(BIN)/selfhost
	@$(foreach file,$(TESTS),./check.sh $< $(file);)

clean:
	rm -rf $(BIN)
	rm -f test/*.out test/*.txt test/*.s
