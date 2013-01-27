;;; awlist.scm -- awget link list

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

;; awget link list.
;; 
;; These methods are exported:
;; 
;;   (add-link obj link)
;;   (rem-link obj id)
;;   (set-done obj id)
;;   (get-list obj)
;;   (get-uncompleted obj)
;;   (clear obj)
;;   (save-list obj file-name)
;;   (load-list obj file-name)
;;


;;; Code:

(load-from-path "dsv-parser.scm")

(define-module (awget awlist)
  #:use-module (oop goops)
  #:use-module (unix dsv-parser)
  #:export (<awlist> add-link
                     rem-link
                     set-done
                     get-list
                     get-uncompleted
                     clear
                     save-list
                     load-list))


;;; Main class

(define-class <awlist> ()
  (link-list
   #:setter set-link-list
   #:getter get-link-list
   #:init-value '())

  (last-record-number
   #:setter set-last-record-number
   #:getter get-last-record-number
   #:init-value 0)

  (mutex
   #:setter set-mutex
   #:getter get-mutex))

(define-method (initialize (obj <awlist>) args)
  (next-method)
  (set-mutex obj (make-mutex)))


;;; Public methods

;; Add new link LINK to the list.
(define-method (add-link (obj <awlist>) (link <string>))
  (lock-mutex (get-mutex obj))
  (let* ((record-number (1+ (get-last-record-number obj)))
         (tstamp        (current-time))
         (record        (list
                         (number->string record-number)
                         (number->string tstamp)
                         "x" link)))

    (set-last-record-number obj record-number)

    (set-link-list obj (append (get-link-list obj) (list record))))

  (unlock-mutex (get-mutex obj)))

;; Mark a link with id ID as done.
(define-method (set-done (obj <awlist>) (id <number>))
  (lock-mutex (get-mutex obj))

  (let ((rec (assoc (number->string id) (get-link-list obj))))
    (if (not (eq? rec #f))
        ;; TODO: Remove hardcoded value
        (list-set! rec 2 (number->string (current-time)))))

  (unlock-mutex (get-mutex obj)))

;; Remove a link with id ID.
(define-method (rem-link (obj <awlist>) (id <number>))
  (lock-mutex (get-mutex obj))
  (set-link-list obj (remove-if (lambda (element) (eq? (car elem) id))
                                (get-link-list obj)))
  (unlock-mutex (get-mutex obj)))

(define-method (get-list (obj <awlist>))
  (lock-mutex (get-mutex obj))
  (let ((link-list (get-link-list obj)))
    (unlock-mutex (get-mutex obj))
    link-list))

;; Get list of uncompleted downloads.
(define-method (get-uncompleted (obj <awlist>))
  (lock-mutex (get-mutex obj))

  (let ((link-list   (get-link-list obj))
        (uncompleted '()))

    (unlock-mutex (get-mutex obj))

    (for-each
     (lambda (record)
       (if (string=? (list-ref record 2) "x")
           (set! uncompleted (cons record uncompleted))))
     link-list)
    uncompleted))

;; Clear the link list.
(define-method (clear (obj <awlist>))
  (lock-mutex (get-mutex obj))
  (set-link-list obj '())
  (unlock-mutex (get-mutex obj)))

;; Save the link list to a file FILE-NAME.
(define-method (save-list (obj <awlist>) (file-name <string>))
  (let ((file-port (open-output-file file-name)))
    (dsv-write (get-link-list obj) file-port)))

;; Load a link list from a file FILE-NAME.
(define-method (load-list (obj <awlist>) (file-name <string>))
  (let ((file-port (open-input-file file-name)))
    (set-link-list obj (dsv-read file-port)))

  (let ((last-rec (car (reverse (get-link-list obj)))))
    (set-last-record-number obj (string->number (car last-rec)))))

;;; awlist.scm ends here.
