BIN := bin
SRC_ROOT := src
SRC_DIRS := ${shell find ${SRC_ROOT} -type d -print}
TESTS := $(wildcard test/*.c)

LD := cc
CC := cc
CCFLAGS := -Wall -pedantic -std=c89 -g
LACCFLAGS := -I /usr/include/x86_64-linux-musl/

# Normal build with gcc
SOURCES := $(foreach sdir,$(SRC_DIRS),$(wildcard $(sdir)/*.c))
OBJECTS := $(patsubst src/%.c,$(BIN)/%.o,$(SOURCES))

# Bootstrap build subset of files
BOOTSTRAP_SOURCES := \
	src/backend/abi.c \
	src/backend/asm.c \
	src/backend/dot.c \
	src/core/cfg.c \
	src/core/cli.c \
	src/core/eval.c \
	src/core/parse.c \
	src/core/string.c \
	src/core/symtab.c \
	src/core/type.c \
	src/frontend/input.c \
	src/frontend/macro.c \
	src/frontend/preprocess.c \
	src/frontend/tokenize.c \
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
