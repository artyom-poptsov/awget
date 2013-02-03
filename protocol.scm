;;; protocol.scm -- awget protocol description.

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

;;
;; awget protocol description.
;;


;;; Code:

(define-module (awget protocol))

(define-public *protocol-version* 1)

(define-public *cmd-get-protocol-version* 0)
(define-public *cmd-add-link*             1)
(define-public *cmd-get-list*             2)
(define-public *cmd-quit*                 3)
(define-public *cmd-rem-link*             4)

;;; protocol.scm ends here
