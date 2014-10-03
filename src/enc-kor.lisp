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
        (ucs (cadr p)))
    (setf (gethash cp949 *cp949-to-ucs-hash*) ucs)
    (setf (gethash ucs *ucs-to-cp949-hash*) cp949)))

(defun ucs->cp949 (code)
  (values (gethash code *ucs-to-cp949-hash*)))

(defun cp949->ucs (code)
  (values (gethash code *cp949-to-ucs-hash*)))

(define-character-encoding :cp949
    "UHC, (CP949, or extended EUC-KR), 1 -or- 2-bytes widely used in Korean."
    :max-units-per-char 2
    :literal-char-code-limit #x80)

(define-octet-counter :cp949 (getter type)
  `(named-lambda cp949-octet-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with noctets fixnum = 0
        for i fixnum from start below end
        for code of-type code-point = (,getter seq i)
        do (let ((new (let ((cp949 (ucs->cp949 code)))
                        (if (null cp949) 1 2))))            
             (incf noctets new)
             (when (and (plusp max) (> new max)) (loop-finish)))
        finally (return (values noctets i)))))
 
(define-code-point-counter :cp949 (getter type)
  `(named-lambda cp949-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (error :NIY-cp949-code-point-counter)))

(define-encoder :cp949 (getter src-type setter dest-type)
  `(named-lambda cp949-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
        for i fixnum from start below end
        for code of-type code-point = (,getter src i)
        for cp949 = (ucs->cp949 code) do
          (macrolet ((set-octet (offset value)
                       `(,',setter ,value dest (the fixnum (+ di ,offset)))))
            (if (<= code #x80)
                ;; 1-byte.
                (progn
                  (set-octet 0 code)
                  (incf di))
                ;; 2-bytes.
                (if (null cp949)
                    ;; unknown-code.
                    (error "skip?" :no-mapping-for-cp949)
                    ;; ok.
                    (progn
                      (set-octet 0 (f-logand #xff (f-ash cp949 -8)))
                      (set-octet 1 (logand cp949 #xff))                       
                      (incf di 2)))))
        finally (return (the fixnum (- di d-start))))))


(define-decoder :cp949 (getter src-type setter dest-type)
  `(named-lambda cp949-decoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (error :NIY-cp949-decoder)))


;;;EOF.
