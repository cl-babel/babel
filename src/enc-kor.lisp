;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-kor.lisp --- Korean encodings.
;;;

(in-package #:babel-encodings)

(defvar +cp949-ascii-max+ #x7f)

(defvar *cp949-to-ucs-hash* (make-hash-table))
(defvar *ucs-to-cp949-hash* (make-hash-table))
(dolist (p +cp949->unicode+)
  (let ((cp949 (car p))
        (ucs (cdr p)))
    (setf (gethash cp949 *cp949-to-ucs-hash*) ucs)
    (setf (gethash ucs *ucs-to-cp949-hash*) cp949)))





;;;EOF.
