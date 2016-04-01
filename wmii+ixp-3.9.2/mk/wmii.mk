VERSION = 3.9.2
CONFVERSION = 
COPYRIGHT = ©2010 Kris Maglione
SUBMAKE_EXPORT = WMII_HGVERSION=""

LIBS9 = $(ROOT)/lib/libregexp9.a $(ROOT)/lib/libbio.a $(ROOT)/lib/libfmt.a $(ROOT)/lib/libutf.a

CFLAGS += '-DVERSION=\"$(VERSION)\"' '-DCOPYRIGHT=\"$(COPYRIGHT)\"' \
	  '-DCONFVERSION=\"$(CONFVERSION)\"' '-DCONFPREFIX=\"$(ETC)\"'
FILTER = sed "s|@CONFPREFIX@|$(ETC)|g; \
	      s|@CONFVERSION@|$(CONFVERSION)|g; \
	      s|@DOCDIR@|$(DOC)|g; \
	      s|@VERSION@|$(VERSION)|g; \
	      s|@LIBDIR@|$(LIBDIR)|g; \
	      s|@BINSH@|$(BINSH)|g; \
	      s|@TERMINAL@|$(TERMINAL)|g;"

