ROOT=.
include $(ROOT)/mk/hdr.mk
include $(ROOT)/mk/wmii.mk

PDIRS = \
	doc	     \
	man	     \
	cmd	     \
	libwmii_hack \
	rc	     \
	alternative_wmiircs

DIRS =	\
	libbio    \
	libfmt	  \
	libregexp \
	libutf	  \
	libixp	  \
	$(PDIRS)

DOCS = README \
       LICENSE

deb-dep:
	IFS=', '; \
	apt-get -qq install build-essential $$(sed -n 's/([^)]*)//; s/^Build-Depends: \(.*\)/\1/p' debian/control)

deb:
	dpkg-buildpackage -rfakeroot -b -nc

include ${ROOT}/mk/dir.mk
INSTDIRS = $(PDIRS)

