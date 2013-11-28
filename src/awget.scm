#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
export GUILE_LOAD_PATH=__DATA_DIR__
main='(module-ref (resolve-module '\''(awget awget)) '\'main')'
exec ${GUILE-guile} -l $0 -c "(apply $main (command-line))" "$@"
!#

;;; awget -- Awget download manager

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

;; Awget is a yet another download manager.  It is aimed for use with
;; Awesome window manager, but unfortunately written in Scheme instead
;; of Lua.
;;
;; Normally awget daemon (awgetd) will be started as soon as you try
;; to add the first link.  But you can start the daemon manually by
;; calling awget with "-d" option.
;;
;; Usage:
;;
;;   awget [ OPTIONS ] [ LINK LINK LINK ... ]
;;
;;   -h, --help         Print usage and exit.
;;   -v, --version      Print program version and exit.
;;   -d, --daemon       Start the awget daemon (awgetd).
;;   -l, --list         List all links.
;;   --debug            Debug mode.  Normally you shouldn't use this.
;;
;; Example:
;;
;;   $ awget -d
;;   $ awget ftp://ftp.gnu.org/gnu/guile/guile-2.0.0.tar.gz
;;
;; These methods are exported:
;;
;;   (main . args)
;;


;;; Code:

(define-module (awget awget)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (awget protocol)
  #:use-module (awget awgetd)
  #:use-module (awget util logger)
  #:export     (<awget> main))

(define *program-name*    "awget")
(define *program-version* "v0.1")


;;; Main class

(define-class <awget> ()
  (awget-home
   #:setter set-home
   #:getter get-home
   #:init-keyword #:home
   #:init-value (string-append (getenv "HOME") "/." *program-name*))

  (awget-pid-file
   #:setter set-pid-file
   #:getter get-pid-file
   #:init-keyword #:awget-pid-file)

  (awget-socket-path
   #:setter set-socket-path
   #:getter get-socket-path
   #:init-keyword #:awget-socket-path)

  (awget-downloads-dir
   #:setter set-downloads-dir
   #:getter get-downloads-dir
   #:init-keyword #:downloads-dir)

  (awget-link-list-file
   #:setter set-link-list-file
   #:getter get-link-list-file)

  (logger
   #:setter set-logger
   #:getter get-logger)
  
  (debug-mode
   #:setter set-debug-mode
   #:getter debug?
   #:init-value #f)
  
  (version
   #:setter set-version
   #:getter get-version
   #:init-value *program-version*))

(define-method (initialize (obj <awget>) args)
  (next-method)

  (set-pid-file       obj (string-append
                           "/var/run/" *program-name*
                           "/" (getenv "USER")
                           "/" *program-name* ".pid"))
  (set-socket-path    obj (string-append (get-home obj) "/" *program-name*))
  (set-link-list-file obj (string-append (get-home obj) "/awlist"))

  (if (not (file-exists? (get-home obj)))
      (mkdir (get-home obj)))
  
  (set-logger obj (make <logger>
                    #:ident    *program-name*
                    #:facility 'user)))


;;; Public methods

(define-method (print-version (obj <awget>))
  (display
   (string-append
    "awget download manager (version " (get-version obj) ")\n")))

(define-method (print-help (obj <awget>))
  (display
   (string-append
    "awget download manager (version " (get-version obj) ")\n\n"
    "Usage:\n"
    "\t" *program-name* " [options]\n"
    "\n"
    "General options:\n"
    "\t" "-h, --help     Print this message and exit.\n"
    "\t" "-v, --version  Print program version and exit.\n"
    "\t" "-d, --daemon   Start the awget daemon (awgetd).\n"
    "\t" "-x, --exit     Stop daemon.\n"
    "\t" "--debug        Debug mode.  Normally you should'n use this.\n"
    "\n"
    "Download management:\n"
    "\t" "-a, --add      Add a new link.\n"
    "\t" "-n, --link     Set current link.\n"
    "\t" "-l, --list     List all links.\n"
    "\t" "-r, --remove   Remove the current link.\n")))

(define-method (daemon-started? (obj <awget>))
  (file-exists? (get-pid-file obj)))


;; Run the awgetd
(define-method (daemonize (obj <awget>))
  (let* ((socket-path (get-socket-path obj))
         (awgetd (make <awgetd>
                   #:debug-mode        (debug?             obj)
                   #:awget-home        (get-home           obj)
                   #:awget-pid-file    (get-pid-file       obj)
                   #:awget-socket-path (get-socket-path    obj)
                   #:link-list-file    (get-link-list-file obj))))
    (display "Starting awgetd...\n")
    (logger-message (get-logger obj) 'info "Starting awgetd...")
    (run awgetd)))

;; List all links in the human-friendly manner.
(define (print-list list)
  (let ((fmt "~5a ~15a ~a\n"))
    (format #t fmt "ID" "Status" "Link")
    (for-each
     (lambda (line)
       (format
        #t
        fmt
        (list-ref line 0)
        (if (string=? (list-ref line 2) "x")
            "Downloading"
            "Done")
        (list-ref line 3)))
     list)))


;; Print the message MESSAGE only if daemon started in debug mode.
(define-method (debug-message (obj <awget>) (message <string>))
  (if (debug? obj)
      (logger-message (get-logger obj) 'debug message)))

;;; Protocol implementation

(define-method (send-message (obj  <awget>) (type <number>) message)
  (let ((message (list type message))
        (server-port (socket PF_UNIX SOCK_STREAM 0)))

    (connect server-port AF_UNIX (get-socket-path obj))

    ;; Send the message
    (display message server-port)
    (newline server-port)

    ;; Receive a response
    (let ((response (read server-port)))
      (close server-port)
      response)))

(define-method (stop-daemon (obj <awget>))
  (send-message obj *cmd-quit* #f))

;; Get a download queue
(define-method (get-list (obj <awget>))
  (send-message obj *cmd-get-list* #f))

;; Send a link LINK to the awgetd.
(define-method (send-link-to-daemon (obj <awget>) (link <string>))
  (send-message obj *cmd-add-link* link))

;; Add list of links to download queue
(define-method (add-link (obj <awget>) (link-list <list>))
  (for-each (lambda (link) (send-link-to-daemon obj link))
            link-list))

(define-method (rem-link (obj <awget>) (link-id <number>))
  (send-message obj *cmd-rem-link* link-id))


;;; Program entry point

;; Parse arguments and run awget
(define (main . args)

  (define *option-spec*
    '(;; General options
      (daemon  (single-char #\d) (value #f))
      (exit    (single-char #\x) (value #f))
      (version (single-char #\v) (value #f))
      (help    (single-char #\h) (value #f))
      (debug                     (value #f))
      ;; Link management
      (add     (single-char #\a) (value #f))
      (link    (single-char #\n) (value #t))
      (list    (single-char #\l) (value #f))
      (remove  (single-char #\r) (value #f))))

  (let* ((awget          (make <awget>))
         (options        (getopt-long args *option-spec*))
         (help-wanted    (option-ref options 'help    #f))
         (version-wanted (option-ref options 'version #f))
         (daemon-wanted  (option-ref options 'daemon  #f))
         (exit-wanted    (option-ref options 'exit    #f))
         (debug-wanted   (option-ref options 'debug   #f))

         (add-link-wanted    (option-ref options 'add    #f))
         (list-wanted        (option-ref options 'list   #f))
         (current-link       (option-ref options 'link   #f))
         (remove-link-wanted (option-ref options 'remove #f))

         (arguments      (option-ref options '()      #f)))

    (if (eq? debug-wanted #t)
        (begin
          (set-debug-mode awget #t)
          (debug-message awget "Debug mode enabled")))

    (cond

     (version-wanted
      (print-version awget))

     (help-wanted
      (print-help awget))

     (daemon-wanted
      (if (not (daemon-started? awget))
          (daemonize awget)
          (display "Daemon already started.\n")))

     (list-wanted
      (if (not (daemon-started? awget))
          (daemonize awget))
      (print-list (get-list awget))
      (newline))

     (exit-wanted
      (if (daemon-started? awget)
          (stop-daemon awget)))

     (add-link-wanted
      (if (not (daemon-started? awget))
          (daemonize awget))
      (add-link awget arguments))

     (remove-link-wanted
      (rem-link awget (string->number current-link))))

    (quit)))

;;; awget ends here
