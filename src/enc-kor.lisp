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


(define-character-encoding :cp949
    "UHC, (CP949, or extended EUC-KR), 1 -or- 2-bytes widely used in Korean."
    :max-units-per-char 2
    :literal-char-code-limit #x80)


(define-octet-counter :cp949 (getter type)
  `(named-lambda cp949-octet-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (error :NIY-cp949-octet-counter)))

; for code of-type code-point = (,getter seq i)
;           for i fixnum from start below end
;           finally (return (values noctets i)))))

(define-code-point-counter :cp949 (getter type)
  `(named-lambda cp949-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (error :NIY-cp949-code-point-counter)))

(define-encoder :cp949 (getter src-type setter dest-type)
  `(named-lambda cp949-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (error :NIY-cp949-encoder)))

(define-decoder :cp949 (getter src-type setter dest-type)
  `(named-lambda cp949-decoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (error :NIY-cp949-decoder)))


;;;EOF.
