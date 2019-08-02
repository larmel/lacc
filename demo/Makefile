.POSIX:
.SUFFIXES:

CC = ../../bin/lacc

help:
	@echo "Choose one of the following targets to demo lacc:"
	@echo ""
	@echo "    git: Build and run tests on the git source code."
	@echo "  quake: Compile ioquake3, a fork of the original Quake source code."
	@echo ""

git: git/.git git/ccwrap.py
	make -C git clean
	make -C git CC="CC=${CC} ./ccwrap.py" COMPUTE_HEADER_DEPENDENCIES=no test

git/ccwrap.py: ccwrap.py
	cp $? $@
	chmod +x $@

git/.git:
	git clone https://github.com/git/git.git

quake: ioq3/.git
	make -C ioq3 CC='${CC} -DSDL_DISABLE_IMMINTRIN_H -D__inline__=inline' GENERATE_DEPENDENCIES=0 USE_CURL=0

ioq3/.git:
	git clone https://github.com/ioquake/ioq3.git

clean:
	make -C git clean
	make -C ioq3 clean

.PHONY: help git quake clean
