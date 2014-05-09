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
   ;; List of downloads.
   #:setter set-link-list!
   #:getter get-link-list
   #:init-value '())
  (last-record-number
   ;; Number of the last added download.
   #:setter set-last-record-number!
   #:getter get-last-record-number
   #:init-value 0)
  (mutex
   ;; Global mutex for critical sections of awlist.  Must be locked
   ;; before doing any potentially concurrent operations on
   ;; `link-list' in public procedures.
   #:getter get-mutex
   #:init-value (make-mutex)))

(define-method (initialize (obj <awlist>) args)
  (next-method))

(define %link-ts-idx 2)                 ;Index of the timestamp
(define %uncompleted-marker "x")

;; Create the list.
(define awlist (make <awlist>))


;;; Public methods

(define (make-record number timestamp link)
  "Create a new record for uncompleted job."
  (list (number->string number)
        (number->string timestamp)
        %uncompleted-marker link))

(define (awlist-add! link)
  "Add new link LINK to the list."
  (lock-mutex (get-mutex awlist))
  (let* ((record-number (1+ (get-last-record-number awlist)))
         (tstamp        (current-time))
         (record        (make-record record-number tstamp link)))

    (set-last-record-number! awlist record-number)

    (set-link-list! awlist (append (get-link-list awlist) (list record))))

  (unlock-mutex (get-mutex awlist)))

(define (awlist-set-done! id)
  "Mark a link with id ID as done."
  (lock-mutex (get-mutex awlist))

  (let ((rec (assoc (number->string id) (get-link-list awlist))))
    (if rec
        (list-set! rec %link-ts-idx (number->string (current-time)))))

  (unlock-mutex (get-mutex awlist)))

(define (awlist-rem! id)
  "Remove a link with id ID."
  (lock-mutex (get-mutex awlist))
  (set-link-list! awlist (remove-if (lambda (element)
                                     (= (string->number (car element)) id))
                                   (get-link-list awlist)))
  (unlock-mutex (get-mutex awlist)))

(define (awlist-get)
  (lock-mutex (get-mutex awlist))
  (let ((link-list (get-link-list awlist)))
    (unlock-mutex (get-mutex awlist))
    link-list))

(define (completed? record)
  "Check if RECORD contains completed download job."
  (not (string=? (list-ref record %link-ts-idx)
                 %uncompleted-marker)))

(define (awlist-get-uncompleted)
  "Get list of uncompleted downloads."
  (lock-mutex (get-mutex awlist))

  (let ((link-list (get-link-list awlist)))

    (unlock-mutex (get-mutex awlist))

    (reduce-init (lambda (prev record)
                   (if (not (completed? record))
                       (cons record prev)
                       prev))
                 '()
                 link-list)))

(define (awlist-clear!)
  "Clear the link list."
  (lock-mutex (get-mutex awlist))
  (set-link-list! awlist '())
  (unlock-mutex (get-mutex awlist)))

(define (awlist-save file-name)
  "Save the link list to a file FILE-NAME."
  (let ((file-port (open-output-file file-name)))
    (dsv-write (get-link-list awlist) file-port)))

(define (awlist-load file-name)
  "Load a link list from a file FILE-NAME."
  (let ((file-port (open-input-file file-name)))
    (set-link-list! awlist (dsv-read file-port)))

  (if (not (null? (get-link-list awlist)))
      (let ((last-rec (car (reverse (get-link-list awlist)))))
        (set-last-record-number! awlist (string->number (car last-rec))))))

;;; awlist.scm ends here.
