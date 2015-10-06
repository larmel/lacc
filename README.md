A simple, self-hosting C compiler
=================================

This is a toy project of mine, motivated by wanting to learn more about
programming in C, and compiler construction.
Beyond that, there is no purpose.
The goal is simply to make a compiler for (a subset of) C, written in C, which
is able to compile itself.

Most language constructs in C89 are supported, in addition to some elements from
later standards.
Notable omissions are bit fields, and anything having to do with floating point
numbers.
Not all features used in glibc headers are supported, so the Makefile assumes
you have [musl](http://www.musl-libc.org/) installed.

Implementation is entirelly C89, using only the standard headers and some POSIX
extensions. There are no external dependencies.

The current version can produce x86\_64 assembly, following the
[System V ABI](http://www.x86-64.org/documentation/abi.pdf) for calling convention and
memory layout.
This means that object files can be linked with files produced by other
compilers.
Use GNU assembler and linker to create the final executable from assembly
output.

Here is compiling "hello world" from terminal, typing in interactive mode:

```
$ bin/lacc -S -o hello.s
int puts(const char *s);

int main(void) {
	puts("Hello World!");
	return 0;
}

$ cat hello.s
	.data
	.align	16
__func__.1:
	.string	"main"
	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$4, %rsp
.L1:
	movq	$.LC0, %rdi
	call	puts
	movl	%eax, -4(%rbp)	# store .t0
	movl	$0, %eax
	leaveq
	retq
	.size	main, .-main
	.section .rodata
.LC0:
	.string "Hello World!"

$ gcc hello.s -o hello && ./hello
Hello World!
```

There is also an option to produce DOT diagrams of the internal CFG
representation:

```c
int main(int argc, char *argv[]) {
	int i, sum = 0;
	for (i = 0; i < argc; ++i) {
		sum += i;
	}
	return sum;
}
```

![Internal representation of for loop](doc/control-flow.png)
