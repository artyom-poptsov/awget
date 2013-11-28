awgetdir = $(datadir)/awget

bin_SCRIPTS	= awget
dist_doc_DATA	= README COPYING AUTHORS ChangeLog
awget_DATA	= src/awgetd.scm src/awlist.scm src/protocol.scm

awget_utildir	= $(pkgdatadir)/util
awget_util_DATA	= src/dsv-parser.scm src/notify-bus.scm \
			src/wget.scm

awget: src/awget.scm
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

CLEANFILES	= awget

