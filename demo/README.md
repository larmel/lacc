# Demos

This directory contains scripts for executing lacc on some interesting C programs other than the compiler itself.
By default the executable under `../bin/` is used, so make sure to build lacc before running these demos.


## Git

Git provides a great test case to validate the compiler, as we can run its more than 20 000 test cases after a successful compile to weed out any miscompilations.
To build and run tests, execute `make git` in this directory.

Running the first time, the script will clone the mirror repository at github, and place it under a `git` folder.
Build and test is done on whatever branch is checked out.

Because of some limitations with the current linker implementation, we cannot use the lacc executable for linker invocations. The git Makefile does not allow separately substituting `CC` and `LD`, so we use `ccwrap.py` to invoke lacc only when compiling C code and not for linking.

	make CC="CC=../../bin/lacc ./ccwrap.py" COMPUTE_HEADER_DEPENDENCIES=no test

For debugging miscompilations, we can run a particular failing test in the `t` directory, for example:

	make -C t t7063-status-untracked-cache.sh

Figuring out what code has been miscompiled can be a bit tedious, but one way to do it is to use binary search over a set of files compiled with lacc; compiling one half with a known good compiler, recursively.
Once we are left with a single file causing a test failure, try to split the file in two until we are left with a single function.
Then it will usually be apparent what goes wrong.

Several bugs in lacc have been identified this way, such as "Evaluate switch expression to avoid side effects" [(eb3453f3)](https://github.com/larmel/lacc/commit/ef6f4c8d98c0473adbd7291591ee4856eb3453f3), "Fix liveness analysis of pointer arguments" [(c2979933)](https://github.com/larmel/lacc/commit/a0e292ebe61fd41709efb1b2728c0f66c2979933), "Ensure side effects in for expressions are evaluated" [(ba785207)](https://github.com/larmel/lacc/commit/c638cfc3150ff7c48a1b7755dd55b152ba785207).


## Quake (ioquake3)

It is possible to play this fork of Quake 3 compiled with lacc.
The source is downloaded into `ioq3` directory on first invocation of `make quake`.
After the build is complete, launch the game through `/build/release-linux-x86_64/ioquake3.x86_64`.
Note that you will need to provide game assets separately.

Some adjustments are needed to the compile flags to make it work.

	make CC=../../bin/lacc -DSDL_DISABLE_IMMINTRIN_H -D__inline__=inline' GENERATE_DEPENDENCIES=0 USE_CURL=0

Define `__inline__` because of a header in a jpeg library being generated while assuming something like GCC; this is not a keyword in lacc.
Define `SDL_DISABLE_IMMINTRIN_H` since we don't have an `immintrin.h` for lacc, which SDL tries to use.
In cURL, there is a header which complains because it cannot detect the compiler, so disable that.

On a technical note, this program is what initiated the implementation of inline assembly in lacc.
See [snapvector.c](https://github.com/ioquake/ioq3/blob/master/code/asm/snapvector.c) for an example of how it is used in the game.
Support for assembly is still quite limited, not much more than what is required to compile Quake.
