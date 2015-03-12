#ifndef HEADER_H
#define HEADER_H

#ifndef FOO
  #define FOO 1
#endif

#undef FOO

#if !defined(FOO) && !(2 * FOO + 1 != 1)
  #define FOO 0
#endif

#endif
