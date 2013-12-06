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


;;; Code:

(define-module (awget awlist)
  #:use-module (oop goops)
  #:use-module (awget util dsv-parser)
  #:use-module (ice-9 common-list)
  #:export (awlist-add!
            awlist-rem!
            awlist-set-done!
            awlist-get
            awlist-get-uncompleted
            awlist-clear!
            awlist-save
            awlist-load))

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

;; Index of the timestamp
(define *link-ts-idx* 2)

;; Create the list.
(define awlist (make <awlist>))


;;; Public methods

(define (awlist-add! link)
  "Add new link LINK to the list."
  (lock-mutex (get-mutex awlist))
  (let* ((record-number (1+ (get-last-record-number awlist)))
         (tstamp        (current-time))
         (record        (list
                         (number->string record-number)
                         (number->string tstamp)
                         "x" link)))

    (set-last-record-number awlist record-number)

    (set-link-list awlist (append (get-link-list awlist) (list record))))

  (unlock-mutex (get-mutex awlist)))

(define (awlist-set-done! id)
  "Mark a link with id ID as done."
  (lock-mutex (get-mutex awlist))

  (let ((rec (assoc (number->string id) (get-link-list awlist))))
    (if rec
        (list-set! rec *link-ts-idx* (number->string (current-time)))))

  (unlock-mutex (get-mutex awlist)))

(define (awlist-rem! id)
  "Remove a link with id ID."
  (lock-mutex (get-mutex awlist))
  (set-link-list awlist (remove-if (lambda (element)
                                     (= (string->number (car element)) id))
                                   (get-link-list awlist)))
  (unlock-mutex (get-mutex awlist)))

(define (awlist-get)
  (lock-mutex (get-mutex awlist))
  (let ((link-list (get-link-list awlist)))
    (unlock-mutex (get-mutex awlist))
    link-list))

(define (awlist-get-uncompleted)
  "Get list of uncompleted downloads."
  (lock-mutex (get-mutex awlist))

  (let ((link-list   (get-link-list awlist))
        (uncompleted '()))

    (unlock-mutex (get-mutex awlist))

    (for-each
     (lambda (record)
       (if (string=? (list-ref record 2) "x")
           (set! uncompleted (cons record uncompleted))))
     link-list)
    uncompleted))

(define (awlist-clear!)
  "Clear the link list."
  (lock-mutex (get-mutex awlist))
  (set-link-list awlist '())
  (unlock-mutex (get-mutex awlist)))

(define (awlist-save file-name)
  "Save the link list to a file FILE-NAME."
  (let ((file-port (open-output-file file-name)))
    (dsv-write (get-link-list awlist) file-port)))

(define (awlist-load file-name)
  "Load a link list from a file FILE-NAME."
  (let ((file-port (open-input-file file-name)))
    (set-link-list awlist (dsv-read file-port)))

  (if (not (null? (get-link-list awlist)))
      (let ((last-rec (car (reverse (get-link-list awlist)))))
        (set-last-record-number awlist (string->number (car last-rec))))))

;;; awlist.scm ends here.
