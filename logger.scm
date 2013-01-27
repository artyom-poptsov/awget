;;; logger.scm -- syslog interface.

;; Copyright (C) 2012-2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Interface for the syslog which uses the logger(1) program. Should
;; work perfectly with a syslog-ng as well as a classic syslog.
;;
;; Usage:
;;   (logger-message obj 'level message)
;;
;; Example:
;;   (let ((logger (make <logger>
;;                   #:ident    "program-name"
;;                   #:facility 'user)))
;;     ...
;;     (logger-message logger 'notice "Hello Scheme World!")


;;; Code:

;;; Module definition

(define-module (unix logger)
  #:use-module (oop goops)
  #:export     (<logger> logger-message))


;;; Main class

(define-class <logger> ()
  (ident
   #:setter set-ident
   #:getter get-ident
   #:init-keyword #:ident)

  (facility
   #:setter set-facility
   #:getter get-facility
   #:init-keyword #:facility))

(define-method (initialize (obj <logger>) args)
  (next-method)
  (if (eqv? (member (get-facility obj) *facility-names*) #f)
      (throw 'logger-wrong-facility (get-facility obj))))


;;; Constants

;; Valid facility names. See logger(1).
(define *facility-names*
  '(auth authpriv cron daemon ftp lpr mail news user uucp
         local1 local2 local3 local4 local5 local6 local7))

;; Valid level names.
(define *level-names*
  '(alert crit debug err info notice warning))
          
;;; Public methods

;; Write a new MESSAGE with LEVEL to the log. LEVEL is expected to be
;; a symbol, and it must be one of the correct levels that syslog
;; understands -- for example, 'info, 'warning, 'error etc.
;;
;; Example of usage:
;;   (logger-message logger-instance 'notice "Hello Scheme World!")
;;
(define-method (logger-message (obj <logger>) (level <symbol>) (message <string>))
  (let ((current-level (member level *level-names*)))
    (if (not (eqv? current-level #f))
        (logger obj (symbol->string (car current-level)) message)
        (throw 'logger-wrong-level level))))

;;; Private methods   

;; Write a new MESSAGE with LEVEL to the syslog. LEVEL is expected to
;; be a string.  Method does no any additional checks for correctness
;; of the LEVEL.
(define-method (logger (obj <logger>) (level <string>) (message <string>))
  (system
   (string-append
    "logger"
    " -t " (get-ident obj)
    " -p " (string-append (symbol->string (get-facility obj)) "." level)
    " -- " "'" message "'")))

;;; logger.scm ends here
