DIRS := ${shell find src -type d -print}
SOURCES := $(foreach sdir,$(DIRS),$(wildcard $(sdir)/*.c))

CFLAGS := -Wall -pedantic -std=c89 -I include/
LACCFLAGS := -I include/

all: bin/lacc

bin/lacc: $(SOURCES)
	@mkdir -p $(dir $@)
	cc $(CFLAGS) $^ -o $@

bin/bootstrap: $(patsubst src/%.c,bin/%-bootstrap.o,$(SOURCES))
	cc $^ -o $@

bin/%-bootstrap.o: src/%.c bin/lacc
	@mkdir -p $(dir $@)
	bin/lacc $(LACCFLAGS) -c $< -o $@

bin/selfhost: $(patsubst src/%.c,bin/%-selfhost.o,$(SOURCES))
	cc $^ -o $@

bin/%-selfhost.o: src/%.c bin/bootstrap
	@mkdir -p $(dir $@)
	bin/bootstrap $(LACCFLAGS) -c $< -o $@

test-%: bin/%
	@$(foreach file,$(wildcard test/*.c),./check.sh $< $(file);)

test: test-lacc

clean:
	rm -rf bin
	rm -f test/*.out test/*.txt test/*.s

.PHONY: all test test-% clean
