;;; awgetd.scm -- Daemon that retrieves URLs.

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is a part of awget.
;;
;; awget is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; awget is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with awget.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Awget daemon that retrieves URLs.
;; 
;; These methods are exported:
;; 
;;   (run)


;;; Code:

(define-module (awget awgetd)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)

  ;; Logging
  #:use-module (logging logger)
  #:use-module (logging rotating-log)
  #:use-module (logging port-log)

  ;; RPC
  #:use-module (rpc rpc)
  #:use-module (rpc rpc server)
  #:use-module (rpc xdr)
  #:use-module (rpc xdr types)
  #:use-module (awget rpc-server)
  #:use-module (awget rpc-types+constants)

  #:use-module (awget awlist)
  #:use-module (awget util notify-bus)
  #:use-module (awget util wget)
  #:export (<awgetd> set-awgetd! run-awgetd))

(define *program-name* "awgetd")


;;; Main class

(define-class <awgetd> ()
  ;; Directories
  (data-home
   ;; User's data directory.
   #:getter get-data-home
   #:init-keyword #:data-home)
  (config-home
   ;; Directory to store configuration files.
   #:getter get-config-home
   #:init-keyword #:config-home)
  (runtime-home
   ;; Directory for temporary files.
   #:getter get-runtime-home
   #:init-keyword #:runtime-home)
  (downloads-dir
   ;; Default directory to save downloaded files.
   #:getter get-downloads-dir
   #:init-keyword #:downloads-dir
   #:init-value #f)

  (awget-pid-file
   #:getter get-pid-file
   #:init-keyword #:awget-pid-file)

  (awget-socket-path
   #:getter get-socket-path
   #:init-keyword #:awget-socket-path)

  (awget-socket
   #:setter set-socket!
   #:getter get-socket)

  (no-detach-mode
   #:getter no-detach?
   #:init-keyword #:no-detach-mode
   #:init-value #f)

  (debug-mode
   #:getter debug?
   #:init-keyword #:debug-mode
   #:init-value   #f)

  (logger
   #:setter set-logger!
   #:getter get-logger)

  (wget
   #:setter set-wget!
   #:getter get-wget)

  (link-list-file
   #:getter get-link-list-file
   #:init-keyword #:link-list-file))

(define-method (initialize (obj <awgetd>) args)
  (next-method)

  (set-logger! obj (make <logger>
                     #:ident    *program-name*
                     #:facility 'daemon))

  (if (not (eq? (get-downloads-dir obj) #f))
      (set-dir-prefix! (get-wget obj) (get-downloads-dir obj)))

  (if (file-exists? (get-link-list-file obj))
      (awlist-load (get-link-list-file obj))))

(define awgetd #f)


;;; Helper procedures

(define-method (setup-logging)
  (let ((lgr       (make <logger>))
        (rotating  (make <rotating-log>
                     #:num-files 1
                     #:size-limit 10000
                     #:file-name (format #f "~a/awget.log"
                                         (get-runtime-home awgetd)))))

    (let ((err (make <port-log> #:port (current-error-port))))
      (if (not (debug? awgetd))
          (begin
            ;; don't want to see warnings or info on the screen!
            (disable-log-level! err 'WARN)
            (disable-log-level! err 'INFO)
            (disable-log-level! err 'DEBUG)))
          (add-handler! lgr err))

    ;; add the handlers to our logger
    (add-handler! lgr rotating)
    ;; make this the application's default logger
    (set-default-logger! lgr)
    (open-log! lgr)))

(define (shutdown-logging)
  (flush-log)   ;; since no args, it uses the default
  (close-log!)  ;; since no args, it uses the default
  (set-default-logger! #f))

;; Log formatters
(define (make-msg-formatter prio)
  "Make a new log formatter with priority PRIO."
  (lambda (fmt . args) (log-msg prio (format #f fmt args))))

(define fmt-info  (make-msg-formatter 'INFO))
(define fmt-debug (make-msg-formatter 'DEBUG))
(define fmt-error (make-msg-formatter 'ERROR))

(define (vector->utf8 v)
  "Convert vector V to UTF-8 string."
  (bytevector->string (u8-list->bytevector (array->list v)) "UTF-8"))

;; Taken from Guile-RPC's grpc-nfs-export.in
(define (list->optional-data-list lst)
  "Return an RPC-usable representation of LST, a list of lists (each
item of LST thus represents an XDR structure)."

  ;; XXX: inefficient
  (fold (lambda (elt result)
          (cons 'TRUE
                (append (list elt) (list result))))
        '(FALSE . #f)
        (reverse lst)))


;;; Public methods

(define (run-awgetd)
  "Run the awget daemon."
  (setup-logging)
  (let ((conf    (string-append (get-config-home awgetd)  "/wgetrc"))
        (logfile (string-append (get-runtime-home awgetd) "/wget.log")))
    (set-wget! awgetd (make <wget>
                        #:config  conf
                        #:logfile logfile)))
  (set-nbus! (make <notify-bus> #:app-name *program-name*))
  (daemonize))

(define (set-awgetd! instance)
  "Set awgetd instance."
  (set! awgetd instance))


;;; Private methods

(define (daemonize)
  "Fork the process and start the main loop."
  (fmt-debug "daemonize: Called.")
  (fmt-debug "awgetd: ~a~%" awgetd)
  (if (no-detach? awgetd)

      (begin
        (fmt-debug "no-detach mode")
        (open-socket)
        (start-aworker)
        (create-pid-file (getpid))
        (run-rpc-server))

      (let ((pid (primitive-fork)))
        (if (zero? pid)
            (begin
              (close-port (current-input-port))
              (close-port (current-output-port))

              (let ((p (open-output-file "/dev/null")))
                (set-current-output-port p)
                (set-current-error-port  p))

              (setsid)

              (register-sighandlers)

              (open-socket)
              (start-aworker)
              (run-rpc-server))
            (begin
              (create-pid-file pid)
              (quit))))))

(define (stop)
  (close-socket)
  (awlist-save (get-link-list-file awgetd))
  (remove-pid-file)
  (quit))


(define (register-sighandlers)
  (define (handler signum)
    (fmt-info "Signal ~a has been received." signum)
    (stop))

  (sigaction SIGINT  handler)
  (sigaction SIGHUP  handler)
  (sigaction SIGTERM handler))


(define (create-pid-file pid)
  (let ((pid-file (open-output-file (get-pid-file awgetd))))
    (write pid pid-file)))

(define (remove-pid-file)
  (delete-file (get-pid-file awgetd)))


(define (open-socket)
  (fmt-debug "open-socket: Called.")
  (set-socket! awgetd (socket PF_UNIX SOCK_STREAM 0))
  (let ((path         (get-socket-path awgetd))
        (awget-socket (get-socket awgetd)))
    (bind awget-socket AF_UNIX path)
    (listen awget-socket 1024)))

(define (close-socket)
  (close (get-socket awgetd))
  (delete-file (get-socket-path awgetd)))


;;; Handlers

(define (add-link-handler url)
  "Add new link LINK to the download queue"
  (fmt-debug "add-link-handler: ~a" url)
  (let ((url (vector->utf8 url)))
    (fmt-debug "New link: ~a" url)
    (awlist-add! url))
  'SUCCESS)

(define (rem-link-handler link-id)
  "Remove link with LINK-ID from the list."
  (fmt-debug "rem-link-handler: ~a~%" link-id)
  (awlist-rem! link-id)
  'SUCCESS)

(define (get-list-handler unused)
  "Get list of links."

  (define (encode lst)
    "Encode awlist to representation that can be transferred by RPC."
    (map (lambda (rec)
           ;; FIXME: ID and timestamps must be stored as numbers.
           (list (string->number     (list-ref rec 0))
                 (string->number     (list-ref rec 1))
                 ;; FIXME: Unfinished downloads should be marked with
                 ;; "-1"
                 (string->number     (if (string=? (list-ref rec 2) "x")
                                         "-1"
                                         (list-ref rec 2)))
                 (string->bytevector (list-ref rec 3) "UTF-8")))
         lst))

  (let* ((link-list     (encode (awlist-get)))
         (enc-link-list (list->optional-data-list link-list)))
    (cons 'SUCCESS (list enc-link-list))))

(define (quit-handler unused)
  (fmt-debug "quit-handler: Called.")
  (awlist-save (get-link-list-file awgetd))
  (shutdown-logging)
  (close-socket)
  (remove-pid-file)
  ;; FIXME: Probably this handler should use one-way RPC call.
  (quit))


;; Asynchronous dowloader

(define (aworker-main-loop awgetd)
  "Constantly look through the queue and download uncompleted links."
  (let ((wget (get-wget awgetd)))

    (define (download record)
      (let ((id   (string->number (list-ref record 0)))
            (link (list-ref record 3)))
        (get-url  wget      link)
        (awlist-set-done! id)
        (notify-send 'low "Download finished:" link)))

    (while #t
      (let ((uncompleted (awlist-get-uncompleted)))
        (if (not (null? uncompleted))
            (begin
              (fmt-debug "Great! Some work should be done.")
              (for-each download uncompleted))
            (begin
              (yield)
              (sleep 1)))))))

(define (start-aworker)
  (make-thread (aworker-main-loop awgetd)))


(define awget-rpc-server
  (make-AWGET-PROGRAM-server
   `(("AWGET_VERSION"
      ("add_link" . ,add-link-handler)
      ("rem_link" . ,rem-link-handler)
      ("get_list" . ,get-list-handler)
      ("quit"     . ,quit-handler)))))

(define (run-rpc-server)
  (fmt-debug "run-rpc-server: Called.")
  (run-stream-rpc-server (list (cons (get-socket awgetd) awget-rpc-server))
                         1000000
                         #f               ;Close connections handler
                         (lambda ()
                           #f)))             ;Idle thunk

;;; awgetd.scm ends here.
