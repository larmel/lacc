BIN := bin
SRC_DIRS := src src/util
TESTS := $(wildcard test/*.c)

LD := cc
CC := cc
CCFLAGS := -Wall -pedantic -std=c89
LCCFLAGS := -I /usr/include/x86_64-linux-musl/

# Normal build with gcc
SOURCES := $(foreach sdir,$(SRC_DIRS),$(wildcard $(sdir)/*.c))
OBJECTS := $(patsubst src/%.c,$(BIN)/%.o,$(SOURCES))

# Bootstrap build subset of files
BOOTSTRAP_SOURCES := \
	src/abi.c \
	src/error.c \
	src/lcc.c \
	src/tokenize.c \
	src/util/memoize.c \
	src/util/list.c
BOOTSTRAP_OBJECTS := $(patsubst src/%.c,$(BIN)/%-bootstrap.o,$(BOOTSTRAP_SOURCES))
REMAINING_SOURCES := $(filter-out $(BOOTSTRAP_SOURCES), $(SOURCES))
REMAINING_OBJECTS := $(patsubst src/%.c,$(BIN)/%.o,$(REMAINING_SOURCES))

.PHONY: all bootstrap test test-bootstrap clean

all: $(BIN)/lcc
bootstrap: $(BIN)/bootstrap

$(BIN)/%.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CCFLAGS) -c $< -o $@

$(BIN)/%-bootstrap.o: $(BIN)/%-bootstrap.s
	@mkdir -p $(dir $@)
	$(CC) -c $< -o $@

$(BIN)/%-bootstrap.s: src/%.c $(BIN)/lcc
	@mkdir -p $(dir $@)
	$(BIN)/lcc $(LCCFLAGS) -S $< -o $@

$(BIN)/lcc: $(OBJECTS)
	$(LD) $^ -o $@

$(BIN)/bootstrap: $(BOOTSTRAP_OBJECTS) $(REMAINING_OBJECTS)
	$(LD) $^ -o $@

test: $(BIN)/lcc
	@$(foreach file,$(TESTS),./check.sh $< $(file);)

test-bootstrap: $(BIN)/bootstrap
	@$(foreach file,$(TESTS),./check.sh $< $(file);)

clean:
	rm -rf $(BIN)
