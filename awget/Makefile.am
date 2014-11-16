bin_SCRIPTS = 		\
	awget		\
	awgetd

RPC = \
	rpc-types+constants.scm		\
	rpc-client.scm			\
	rpc-server.scm

awgetdir   = $(datadir)/awget
awget_DATA = \
	awlist.scm			\
	config.scm			\
	notify-bus.scm 			\
	wget.scm 			\
	$(RPC)

EXTRA_DIST = awget.in awgetd.in config.scm.in

GRPCC = `which grpc-compile`

substitute = sed -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g'

SUFFIXES = .in

.in:
	$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

awget: awget.in
	sed -e 's|__DATA_DIR__|$(datadir)|g' $< > $@
	chmod +x $@

awgetd: awgetd.in
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

clean-rpc: $(RPC)
	-$(RM) $(RPC)
.PHONY: clean-rpc

CLEANFILES = \
	awget $(RPC) config.scm

