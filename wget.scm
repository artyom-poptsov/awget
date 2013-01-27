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

;; Interface for wget(1).
;; 
;; These methods are exported:
;;
;;   (get-url url)


;;; Code:

(define-module (unix wget)
  #:use-module (oop goops)
  #:export     (<wget> get-url))


;;; Main class

(define-class <wget> ()
  (proxy
   #:setter set-proxy
   #:getter get-proxy
   #:init-value #f
   #:init-keyword #:proxy)
 
  (downloads-dir
   #:setter set-downloads-dir
   #:getter get-downloads-dir
   #:init-keyword #:downloads-dir)

  (log
   #:setter set-log
   #:getter get-log
   #:init-keyword #:log))

(define-method (initialize (obj <wget>) args)
  (next-method)
  (set-log obj (string-append (getenv "HOME")
                              "/downloads/awget.log"))
  (set-downloads-dir obj
                     (string-append (getenv "HOME")
                                    "/downloads/")))


;;; Public methods

(define-method (get-url (obj <wget>) (url <string>))
  (system (string-append
           "wget"
           " --no-verbose"
           " --append-output=" (get-log obj)
           " --directory-prefix=" (get-downloads-dir obj)
           " " url)))

;;; wget.scm ends here
