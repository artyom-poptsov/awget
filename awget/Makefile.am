bin_SCRIPTS	= awget

awgetdir   = $(datadir)/awget
awget_DATA = \
	awgetd.scm awlist.scm \
	rpc-types+constants.scm rpc-client.scm rpc-server.scm

awget_utildir	= $(pkgdatadir)/util
awget_util_DATA	= dsv-parser.scm notify-bus.scm wget.scm

GRPCC = `which grpc-compile`

awget: awget.in
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

## Generating of RPC files.

rpc-types+constants.scm: rpc.x
	echo ';;; -*- buffer-read-only: t; -*-' > $@
	echo ';;; Automatically generated -- do not modify.' >> $@
	echo '(define-module (awget rpc-types+constants)' >> $@
	echo '  #:use-module (rpc xdr types)' >> $@
	echo '  #:use-module (rpc xdr))' >> $@
	$(GRPCC) --xdr --constants < $< |\
		sed 's|^(define |(define-public |g' >> $@

rpc-client.scm: rpc.x
	echo ';;; -*- buffer-read-only: t; -*-' > $@
	echo ';;; Automatically generated -- do not modify.' >> $@
	echo '(define-module (awget rpc-client)' >> $@
	echo '  #:use-module (awget rpc-types+constants)' >> $@
	echo '  #:use-module (rpc rpc)' >> $@
	echo '  #:use-module (rpc xdr types))' >> $@
	$(GRPCC) --client < $< |\
		sed 's|^(define |(define-public |g' >> $@

rpc-server.scm: rpc.x
	echo ';;; -*- buffer-read-only: t; -*-' > $@
	echo ';;; Automatically generated -- do not modify.' >> $@
	echo '(define-module (awget rpc-server)' >> $@
	echo '  #:use-module (awget rpc-types+constants)' >> $@
	echo '  #:use-module (srfi srfi-1)' >> $@
	echo '  #:use-module (rpc rpc server)' >> $@
	echo '  #:use-module (rpc xdr types))' >> $@
	$(GRPCC) --server < $< |\
		sed 's|^(define |(define-public |g' >> $@

RPC = \
	rpc-types+constants.scm		\
	rpc-client.scm			\
	rpc-server.scm

clean-rpc:
	-$(RM) -f $(RPC)
.PHONY: clean-rpc

CLEANFILES = \
	awget $(RPC)

