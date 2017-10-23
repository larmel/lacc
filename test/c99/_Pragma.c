#define INCLUDE(x) PRAGMA(include #x)
#define PRAGMA(x) _Pragma(#x)

_Pragma("$")
_Pragma(
	"\t" /*
	Something */
)
_Pragma
(""
)

int _Pragma ( "lacc \"..\\file.c\"" ) i = 42;

INCLUDE ( ..\file.c )

int main(void) {
	return i;
}
