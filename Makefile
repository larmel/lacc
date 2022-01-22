.POSIX:
.SUFFIXES:

include config.mak

bin/lacc: $(SOURCES) $(HEADERS) config.h config.mak
	mkdir -p $(@D)
	$(CC) $(CPPFLAGS) $(CFLAGS) -Iinclude -include config.h -DAMALGAMATION src/lacc.c -o $@

install:
	install -d -m 755 $(DESTDIR)$(BINDIR)
	install -d -m 755 $(DESTDIR)$(LIBDIR)/lacc/include
	install -m 755 bin/lacc $(DESTDIR)$(BINDIR)
	install -m 644 $(LIBS) $(DESTDIR)$(LIBDIR)/lacc/include

uninstall:
	rm -rf $(DESTDIR)$(LIBDIR)/lacc
	rm -f $(DESTDIR)$(BINDIR)/lacc

clean:
	rm -rf bin

distclean: clean
	rm -f config.h config.mak

.PHONY: install uninstall clean distclean
