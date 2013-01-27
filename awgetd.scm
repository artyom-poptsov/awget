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

(load-from-path "protocol.scm")
(load-from-path "logger.scm")
(load-from-path "notify-bus.scm")
(load-from-path "wget.scm")
(load-from-path "awlist.scm")

(define-module (awget awgetd)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (awget protocol)
  #:use-module (awget awlist)
  #:use-module (unix logger)
  #:use-module (unix notify-bus)
  #:use-module (unix wget)
  #:export (<awgetd> run))

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

  (debug-mode
   #:init-keyword #:debug-mode
   #:init-value   #f
   #:getter debug?)

  (logger
   #:setter set-logger
   #:getter get-logger)

  (notify-bus
   #:setter set-notify-bus
   #:getter get-notify-bus)

  (wget
   #:setter set-wget
   #:getter get-wget)

  (wget-log
   #:setter set-wget-log
   #:getter get-wget-log)

  (link-list
   #:setter set-link-list
   #:getter get-link-list)

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

  (set-link-list obj (make <awlist>))

  (set-wget-log obj (string-append (get-home obj) "/wget.log"))
  (set-wget     obj (make <wget> #:log (get-wget-log obj)))

  (if (file-exists? (get-link-list-file obj))
      (load-list (get-link-list obj) (get-link-list-file obj))))


;;; Public methods

;; Run the awget daemon.
(define-method (run (obj <awgetd>))
  (daemonize obj))


;;; Private methods

;; Fork the process and start the main loop.
(define-method (daemonize (obj <awgetd>))
  (let ((pid (primitive-fork)))
  (if (zero? pid)
      (begin
        (setsid)
        (open-socket   obj)
        (start-aworker obj)
        (main-loop     obj))
      (begin
        (create-pid-file obj pid)
        (quit)))))

(define-method (stop (obj <awgetd>))
  (close-socket obj)
  (save-list (get-link-list obj) (get-link-list-file obj))
  (remove-pid-file obj)
  (quit))


(define-method (create-pid-file (obj <awgetd>) (pid <number>))
  (let ((pid-file (open-output-file (get-pid-file obj))))
    (write pid pid-file)))

(define-method (remove-pid-file (obj <awgetd>))
  (delete-file (get-pid-file obj)))


;; Print the message MESSAGE only if daemon started in debug mode.
(define-method (debug-message (obj <awgetd>) (message <string>))
  (if (debug? obj)
      (logger-message (get-logger obj) 'debug message)))


(define-method (open-socket (obj <awgetd>))
  (set-socket obj (socket PF_UNIX SOCK_STREAM 0))
  (let ((path         (get-socket-path obj))
        (awget-socket (get-socket obj)))
    (bind awget-socket AF_UNIX path)
    (listen awget-socket 1)))

(define-method (close-socket (obj <awgetd>))
  (close (get-socket obj))
  (delete-file (get-socket-path obj)))


;; Add new link LINK to the download queue
(define-method (add-link (obj <awgetd>) (link <string>))
  (debug-message obj (string-append "New link: " link))
  (add-link (get-link-list obj) link))

(define-method (send-message (obj <awgetd>) message (port <port>))
  (write message port)
  (newline port))


;; Asynchronous dowloader

(define-method (start-aworker (obj <awgetd>))
  (make-thread (aworker-main-loop obj)))

;; Constantly look through the queue and download uncompleted links.
(define-method (aworker-main-loop (obj <awgetd>))
  (let ((link-list  (get-link-list obj))
        (wget       (get-wget obj))
        (notify-bus (get-notify-bus obj)))

    (define (download record)
      (let ((id   (string->number (list-ref record 0)))
            (link (list-ref record 3)))
        (get-url  wget      link)
        (set-done link-list id)
        (notify-send notify-bus 'low "Download finished:" link)))

    (while #t
      (let ((uncompleted (get-uncompleted link-list)))
        (if (not (null? uncompleted))
            (begin
              (debug-message obj "Great! Some work should be done.")
              (for-each download uncompleted))
            (begin
              (yield)
              (sleep 1)))))))


;; Main loop of the awgetd
(define-method (main-loop (obj <awgetd>))
  (logger-message (get-logger obj) 'info "Daemon started.")

  (let ((awget-socket (get-socket obj)))

    (while #t
      (let* ((client-connection (accept awget-socket))
             (client            (car client-connection))
             (message           (read (open-input-string (read-line client 'trim))))
             (message-type      (car message)))

        (debug-message obj (string-append
                            "Message type: " (number->string message-type)))

        (cond

         ((eq? message-type *cmd-get-protocol-version*)
          (send-message *cmd-get-protocol-version* client))

         ((eq? message-type *cmd-add-link*)
          (let ((message-body (cdr message)))
            (send-message obj #t client)
            (add-link obj (object->string (car message-body)))))

         ((eq? message-type *cmd-get-list*)
          (send-message obj
                        (get-list (get-link-list obj)) client))

         ((eq? message-type *cmd-quit*)
          (begin
            (close client)
            (break))))

        (close client))))

  (stop obj))

;;; awgetd.scm ends here.
