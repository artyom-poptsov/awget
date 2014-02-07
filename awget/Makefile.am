bin_SCRIPTS	= awget

awgetdir	= $(datadir)/awget
awget_DATA	= awgetd.scm awlist.scm protocol.scm

awget_utildir	= $(pkgdatadir)/util
awget_util_DATA	= dsv-parser.scm notify-bus.scm wget.scm

awget: awget.scm
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

CLEANFILES	= awget
