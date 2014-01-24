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

  ;; Logging
  #:use-module (logging logger)
  #:use-module (logging rotating-log)
  #:use-module (logging port-log)

  #:use-module (awget protocol)
  #:use-module (awget awlist)
  #:use-module (awget util notify-bus)
  #:use-module (awget util wget)
  #:export (<awgetd> set-awgetd! run-awgetd))

(define *program-name* "awgetd")


;;; Main class

(define-class <awgetd> ()
  (awget-home
   #:setter set-home
   #:getter get-home
   #:init-keyword #:awget-home)

  (awget-pid-file
   #:setter set-pid-file
   #:getter get-pid-file
   #:init-keyword #:awget-pid-file)

  (awget-socket-path
   #:setter set-socket-path
   #:getter get-socket-path
   #:init-keyword #:awget-socket-path)

  (awget-socket
   #:setter set-socket
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
   #:setter set-logger
   #:getter get-logger)

  (notify-bus
   #:setter set-notify-bus
   #:getter get-notify-bus)

  (wget
   #:setter set-wget
   #:getter get-wget)

  (downloads-dir
   #:setter set-downloads-dir
   #:getter get-downloads-dir
   #:init-keyword #:downloads-dir
   #:init-value #f)

  (link-list-file
   #:setter set-link-list-file
   #:getter get-link-list-file
   #:init-keyword #:link-list-file))

(define-method (initialize (obj <awgetd>) args)
  (next-method)

  (set-logger obj (make <logger>
                    #:ident    *program-name*
                    #:facility 'daemon))

  (set-notify-bus obj (make <notify-bus>
                    #:app-name *program-name*))

  (set-wget obj (make <wget>))
  (if (not (eq? (get-downloads-dir obj) #f))
      (set-dir-prefix (get-wget obj) (get-downloads-dir obj)))

  (if (file-exists? (get-link-list-file obj))
      (awlist-load (get-link-list-file obj))))

(define awgetd #f)


;;; Helper procedures

(define-method (setup-logging)
  (let ((lgr       (make <logger>))
        (rotating  (make <rotating-log>
                     #:num-files 1
                     #:size-limit 10000
                     #:file-name "/tmp/awget.log")))

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


;;; Public methods

(define (run-awgetd)
  "Run the awget daemon."
  (setup-logging)
  (daemonize))

(define (set-awgetd! instance)
  "Set awgetd instance."
  (set! awgetd instance))


;;; Private methods

(define (daemonize)
  "Fork the process and start the main loop."
  (if (no-detach? awgetd)

      (begin
        (open-socket)
        (start-aworker)
        (create-pid-file (getpid))
        (main-loop))

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

              (open-socket )
              (start-aworker)
              (main-loop))
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
  (set-socket awgetd (socket PF_UNIX SOCK_STREAM 0))
  (let ((path         (get-socket-path awgetd))
        (awget-socket (get-socket awgetd)))
    (bind awget-socket AF_UNIX path)
    (listen awget-socket 1)))

(define (close-socket)
  (close (get-socket awgetd))
  (delete-file (get-socket-path awgetd)))


(define (add-link link)
  "Add new link LINK to the download queue"
  (fmt-debug "New link: ~a" link)
  (awlist-add! link))

(define (rem-link link-id)
  (awlist-rem! link-id))

(define (send-message message port)
  (write message port)
  (newline port))


;; Asynchronous dowloader

(define (aworker-main-loop awgetd)
  "Constantly look through the queue and download uncompleted links."
  (let ((wget       (get-wget awgetd))
        (notify-bus (get-notify-bus awgetd)))

    (define (download record)
      (let ((id   (string->number (list-ref record 0)))
            (link (list-ref record 3)))
        (get-url  wget      link)
        (awlist-set-done! id)
        (notify-send notify-bus 'low "Download finished:" link)))

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


(define (main-loop)
  "Main loop of the awgetd"

  (define (awget-accept socket)
    "Wrapper for accept() that catch errors."
    (catch 'system-error
      (lambda ()
        (accept socket))
      (lambda (key . args)
        (fmt-error "accept() call was interrupted."))))

  (fmt-info "Daemon started.")

  (let ((awget-socket (get-socket awgetd)))

    (while #t
      (let* ((client-connection (awget-accept awget-socket))
             (client            (car client-connection))
             (message           (read (open-input-string (read-line client 'trim))))
             (message-type      (car message)))

        (fmt-debug "Message type: ~a" message-type)

        (cond

         ((eq? message-type *cmd-get-protocol-version*)
          (send-message *cmd-get-protocol-version* client))

         ((eq? message-type *cmd-add-link*)
          (let ((message-body (cdr message)))
            (send-message #t client)
            (add-link (object->string (car message-body)))))

         ((eq? message-type *cmd-get-list*)
          (send-message (awlist-get) client))

         ((eq? message-type *cmd-rem-link*)
          (let ((link-id (cadr message)))
            (rem-link link-id)))

         ((eq? message-type *cmd-quit*)
          (begin
            (shutdown-logging)
            (close client)
            (break))))

        (close client))))

  (stop))

;;; awgetd.scm ends here.
