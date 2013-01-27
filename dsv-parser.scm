;;; dsv-parser.scm -- DSV parser.

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

;;
;; Parsers for delimiter separated values (DSV) format that widespread
;; in the Unix world.  Notable example of DSV is /etc/passwd file.
;; Default delimiter is set to a colon.
;;
;; Some examples:
;;
;;   (dsv-string->list "a:b:c")
;;   => '("a" "b" "c")
;;
;;   (dsv-string->list "a;b;c" #\;)
;;   => '("a" "b" "c")
;;
;;   (dsv-string-split "car:cdr:ca\\:dr" #\:)
;;   => ("car" "cdr" "ca\\:dr")
;;
;;   (list->dsv-string '("a" "b" "c"))
;;   => "a:b:c"
;;
;;   (dsv-read (open-input-file "/etc/passwd"))
;;   => (...
;;       ("news" "x" "9" "13" "news" "/usr/lib/news" "/bin/false")
;;       ("root" "x" "0" "0" "root" "/root" "/bin/zsh"))
;;
;; These procedures are exported:
;; 
;;   (dsv-string->list string . delim)
;;   (list->dsv-string list . delim)
;;   (dsv-read . args)
;;   (dsv-write . args)
;;


;;; Code:

(define-module (unix dsv-parser)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)

  ;; escape-special-chars
  #:use-module (string transform)

  #:export (dsv-string->list
            list->dsv-string
            dsv-read
            dsv-write
            dsv-string-split))

;; Default delimiter for DSV
(define *default-delimiter* #\:)


;; Convert DSV from a string STRING to a list.  If delimiter DELIM is
;; not set, use the default delimiter (colon).  Return newly created
;; list.
;;
;; Syntax:
;;
;;   dsv-string->list string [delim]
;;
;; Examples:
;;
;;   (dsv-string->list "a:b:c")
;;   => '("a" "b" "c")
;;
;;   (dsv-string-split "car:cdr:ca\\:dr" #\:)
;;   => ("car" "cdr" "ca\\:dr")
;;
(define (dsv-string->list string . delim)
  (let ((delimiter (if (not (null? delim))
                       (car delim)
                       *default-delimiter*)))
    (dsv-string-split string delimiter)))


;; Convert a list LIST to DSV string.  If delimitter DELIM is not set,
;; use the default delimiter (colon).  Return a DSV string.
;;
;; Syntax:
;;
;;   list->dsv-string list [delim]
;;
;; Example:
;;
;;   (list->dsv-string '("a" "b" "c"))
;;   => "a:b:c"
;;
(define (list->dsv-string list . delim)
  (let ((delimiter (if (not (null? delim))
                     (car delim)
                       *default-delimiter*)))
    (let append-field ((rec list))
      (string-append
       (escape-special-chars (car rec) delimiter #\\)
       (if (not (null? (cdr rec)))
           (string-append
            (string delimiter)
            (append-field (cdr rec)))
           "")))))


;; Read DSV from port PORT.  If port is not set, read from default
;; input port.  If delimiter DELIM is not set, use the default
;; delimiter (colon). Return a list of values.
;;
;; Syntax:
;;
;;   dsv-read [port [delim]]
;;
(define (dsv-read . args)
  (let ((port      (if (not (null? args))
                       (car args)
                       (current-input-port)))
        (delimiter (if (and (not (null? args))
                            (not (null? (cdr args))))
                       (cadr args)
                       *default-delimiter*)))
    (let parse ((dsv-list '()))
      (let ((line (read-line port)))
        (if (not (eof-object? line))
            (parse (cons (dsv-string-split line delimiter) dsv-list))
            (begin
              (set! dsv-list
                    (map
                     (lambda (dsv-data)
                       (map (lambda (field)
                              (regexp-substitute/global
                               #f "\\\\:" field 'pre ":" 'post))
                            dsv-data))
                     dsv-list))
              (reverse dsv-list)))))))


;; Write a list LIST of values as DSV to a port PORT.  If port is not
;; set, write to default output port.  If delimiter DELIM is not set,
;; use the default delimiter (colon).
;;
;; Syntax:
;;
;;   dsv-write list [port [delim]]
;;
(define (dsv-write list . args)
  (let ((port      (if (not (null? args))
                       (car args)
                       (current-output-port)))
        (delimiter (if (and (not (null? args))
                            (not (null? (cdr args))))
                       (cadr args)
                       *default-delimiter*)))

    (let ((dsv (map (lambda (data)
                      (if (not (null? data))
                          (list->dsv-string data delimiter)))
                    list)))
      (for-each
       (lambda (dsv-record)
         (begin
           (display dsv-record port)
           (newline port)))
       dsv))))


;; Split the string STRING into the list of the substrings delimited
;; by appearances of the delimiter DELIM.  If delimiter DELIM is not
;; set, use the default delimiter (colon).
;;
;; This procedure is simlar to string-split, but works correctly with
;; escaped delimiter -- that is, skips it. E.g.:
;;
;; (dsv-string-split "car:cdr:ca\\:dr" #\:)
;; => ("car" "cdr" "ca\\:dr")
;;
;; TODO: Probably the procedure should be rewritten or replaced with
;;       some standard procedure.
;;
;; Syntax:
;;
;;   dsv-string-split string [delim]
;;
(define (dsv-string-split string . delim)
  (let ((delimiter (if (not (null? delim))
                       (car delim)
                       *default-delimiter*))
        (dsv-list  '()))

    (let parse ((start 0)
                (end   1))
      (if (< end (string-length string))
          (begin
            (if (and (eq? (string-ref string end) delimiter)
                     (not (eq? (string-ref string (1- end)) #\\)))
                (begin
                  (set! dsv-list
                        (append dsv-list (list (substring string start end))))
                  (set! start (1+ end))
                  (set! end   (1+ start))
                  (parse start end))
                (parse start (1+ end))))
          (begin
            (set! dsv-list
                  (append dsv-list (list (substring string start end))))
            dsv-list)))))

;;; dsv-parser.scm ends here.
