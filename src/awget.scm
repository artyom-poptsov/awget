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
  #:export     (main))

(define *program-name*    "awget")
(define *program-version* "v0.1")

;;; Honor the XDG specs
;; <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>

(define awget-data-home
  (format #f "~a/.local/share/~a" (getenv "HOME") *program-name*))

(define awget-config-home
  (format #f "~a/.config/~a" (getenv "HOME") *program-name*))

(define awget-runtime-home
  (format #f "~a/.cache/~a" (getenv "HOME") *program-name*))

;;;

(define socket-file
  (string-append awget-runtime-home "/" *program-name*))

(define awlist-file
  (string-append awget-data-home "/awlist"))

(define pid-file
  (string-append
   "/var/run/" *program-name*
   "/" (getenv "USER")
   "/" *program-name* ".pid"))

(define debug-mode?     #f)
(define no-detach-mode? #f)


;;; Helper procedures

(define (print-pathes)
  "Print pathes used by the program."
  (display   "----- pathes -----\n")
  (format #t "data-home:    ~a~%" awget-data-home)
  (format #t "config-home:  ~a~%" awget-config-home)
  (format #t "runtime-home: ~a~%" awget-runtime-home)
  (format #t "socket-file:  ~a~%" socket-file)
  (format #t "awlist-file:  ~a~%" awlist-file)
  (display   "------------------\n"))

(define (print-list list)
  "List all links in the human-friendly manner."
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

(define (debug-message message)
  "Print the message MESSAGE only if daemon started in debug mode."
  (if debug-mode?
      (display message)))

(define (make-all-dirs)
  (define (mkdir-if-not-exists dir)
    (if (not (file-exists? dir)) (mkdir dir)))

  (mkdir-if-not-exists awget-data-home)
  (mkdir-if-not-exists awget-config-home)
  (mkdir-if-not-exists awget-runtime-home))


;;; Procedures

(define (print-version)
  (display
   (string-append
    "awget download manager (version " *program-version* ")\n")))

(define (print-help)
  (display
   (string-append "\
awget download manager (version " *program-version* ")

Usage: " *program-name* " [options]

General options:
  --help, -h             Print this message and exit.
  --version, -v          Print program version and exit.
  --daemon, -d           Start the awget daemon (awgetd).
  --exit, -x             Stop daemon.
  --debug                Debug mode.  Normally you should not use this.
  --no-detach            No-Detach mode.  Normally you should not use this.

Download management:
  --add, -a              Add a new link.
  --link, -n             Set current link.
  --list, -l             List all links.
  --remove, -r           Remove the current link.
")))

(define (daemon-started?)
  "Check if awgetd is started."
  (file-exists? pid-file))


(define (daemonize)
  "Run the awgetd"
  (let* ((awgetd (make <awgetd>
                   #:no-detach-mode    no-detach-mode?
                   #:debug-mode        debug-mode?
                   #:awget-home        awget-data-home
                   #:awget-pid-file    pid-file
                   #:awget-socket-path socket-file
                   #:link-list-file    awlist-file)))
    (set-awgetd! awgetd)
    (display "Starting awgetd...\n")
    (run-awgetd)))

;;; Protocol implementation

(define (send-message type message)
  (let ((message (list type message))
        (server-port (socket PF_UNIX SOCK_STREAM 0)))

    (connect server-port AF_UNIX socket-file)

    ;; Send the message
    (display message server-port)
    (newline server-port)

    ;; Receive a response
    (let ((response (read server-port)))
      (close server-port)
      response)))

(define (stop-daemon)
  (send-message *cmd-quit* #f))

(define (get-list)
  "Get a download queue"
  (send-message *cmd-get-list* #f))

(define (send-link-to-daemon link)
  "Send a link LINK to the awgetd."  
  (send-message *cmd-add-link* link))

(define (add-link link-list)
  "Add list of links to download queue"
  (for-each (lambda (link) (send-link-to-daemon link))
            link-list))

(define (rem-link link-id)
  (send-message *cmd-rem-link* link-id))


;;; Program entry point

(define (main . args)
  "Entry point of the program."

  (define *option-spec*
    '(;; General options
      (daemon  (single-char #\d) (value #f))
      (exit    (single-char #\x) (value #f))
      (version (single-char #\v) (value #f))
      (help    (single-char #\h) (value #f))
      (debug                     (value #f))
      (no-detach                 (value #f))
      ;; Link management
      (add     (single-char #\a) (value #f))
      (link    (single-char #\n) (value #t))
      (list    (single-char #\l) (value #f))
      (remove  (single-char #\r) (value #f))))

  (make-all-dirs)

  (let* ((options        (getopt-long args *option-spec*))
         (help-wanted    (option-ref options 'help    #f))
         (version-wanted (option-ref options 'version #f))
         (daemon-wanted  (option-ref options 'daemon  #f))
         (exit-wanted    (option-ref options 'exit    #f))
         (debug-wanted   (option-ref options 'debug   #f))
         (no-detach-wanted (option-ref options 'no-detach #f))

         (add-link-wanted    (option-ref options 'add    #f))
         (list-wanted        (option-ref options 'list   #f))
         (current-link       (option-ref options 'link   #f))
         (remove-link-wanted (option-ref options 'remove #f))

         (arguments      (option-ref options '()      #f)))

    (if (eq? debug-wanted #t)
        (begin
          (set! debug-mode? #t)
          (debug-message "Debug mode enabled.\n")))

    (if no-detach-wanted
        (begin
          (set! no-detach-mode? #t)
          (debug-message "No-Detach mode enabled.\n")))

    (if debug-mode?
        (print-pathes))

    (cond

     (version-wanted
      (print-version)
      (exit))

     (help-wanted
      (print-help)
      (exit))

     (daemon-wanted
      (if (not (daemon-started?))
          (daemonize)
          (display "Daemon already started.\n")))

     (list-wanted
      (if (not (daemon-started?))
          (daemonize awget))
      (print-list (get-list))
      (newline))

     (exit-wanted
      (if (daemon-started?)
          (stop-daemon)))

     (add-link-wanted
      (if (not (daemon-started?))
          (daemonize))
      (add-link arguments))

     (remove-link-wanted
      (if current-link
          (rem-link (string->number current-link))
          (error "No link is selected."))))

    (quit)))

;;; awget ends here
