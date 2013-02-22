;;; notify-bus.scm -- Interface to libnotify

;; Copyright (C) 2013 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Interface for libnotify.
;; 
;; These methods are exported:
;; 
;;   (notify-send notify-bus urgency-level header message)


;;; Code:

(define-module (unix notify-bus)
  #:use-module (oop goops)
  #:export     (<notify-bus> notify-send))


;;; Main class

(define-class <notify-bus> ()
  (app-name
   #:setter set-app-name
   #:getter get-app-name
   #:init-value #f
   #:init-keyword #:app-name)

  (icon
   #:setter set-icon
   #:getter get-icon
   #:init-value #f
   #:init-keyword #:icon))

;; Valid urgency levels.  See 'notify-send --help'.
(define *urgency-level-names*
  '(low normal critical))


;;; Public methods

;; Send notification with urgency URGENCY-LEVEL.  Message consists of
;; header HEADER and message MESSAGE.
(define-method (notify-send (obj           <notify-bus>)
                            (urgency-level <symbol>)
                            (header        <string>)
                            (message       <string>))
  (let ((current-level (member urgency-level *urgency-level-names*)))
    (if (not (eqv? current-level #f))
        (notify obj (symbol->string (car current-level)) header message)
        (throw 'notify-bus-wrong-level urgency-level))))


;;; Private methods

(define-method (notify (obj           <notify-bus>)
                       (urgency-level <string>)
                       (header        <string>)
                       (message       <string>))
  (system
   (string-append
    "notify-send"
    (if (not (eqv? (get-app-name obj) #f))
        (string-append " --app-name=" (get-app-name obj))
        "")
    " --urgency=" urgency-level
    (if (not (eqv? (get-icon obj) #f))
        (string-append " --icon=" (get-icon obj))
        "")
    " \"" header "\""
    " \"" message "\"")))

;;; notify-bus.scm ends here