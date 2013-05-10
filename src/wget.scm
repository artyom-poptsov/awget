;;; wget.scm -- Interface for wget(1)

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

;; Interface for wget(1).  By default wget will use settings from
;; either system-wide /etc/wgetrc or user's $HOME/.wgetrc (see the GNU
;; Info entry for wget).  This behaviour might be changed by setting
;; #:config slot for <wget>.
;;
;; These methods are exported:
;;
;;   (get-url obj url)
;;   (set-downloads-dir obj path)


;;; Code:

(define-module (awget util wget)
  #:use-module (oop goops)
  #:export     (<wget> get-url
                       set-dir-prefix))


;;; Main class

(define-class <wget> ()
  (dir-prefix
   #:setter set-dir-prefix
   #:getter get-dir-prefix
   #:init-value #f
   #:init-keyword #:dir-prefix)

  (config
   #:setter set-config
   #:getter get-config
   #:init-value #f
   #:init-keyword #:config))

(define-method (initialize (obj <wget>) args)
  (next-method))


;;; Public methods

(define-method (get-url (obj <wget>) (url <string>))
  (system (string-append
           "wget"

           (if (not (eq? (get-config obj) #f))
               (string-append
                " --config=" (get-config obj))
               "")

           (if (not (eq? (get-dir-prefix obj) #f))
               (string-append
                " --directory-prefix=" (get-dir-prefix obj))
               "")

           " '" url "'")))

;;; wget.scm ends here
