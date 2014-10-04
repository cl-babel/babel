;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-kor.lisp --- Korean encodings.
;;;

(in-package #:babel-encodings)


;;; code-table macros.

(defmacro X->UCS (code-table x->ucs-table ucs->x-table 
                  x->ucs-func-name ucs->x-func-name)
  `(progn 
     ;; table mappings.
     (dolist (p ,code-table)
       (let ((X (car p))
             (U (cadr p)))
         (setf (gethash X ,x->ucs-table) U)
         (setf (gethash U ,ucs->x-table) X)))
     ;; helper functions.
     (defun ,ucs->x-func-name (code)
       (values (gethash code ,ucs->x-table)))
     (defun ,x->ucs-func-name (code)
       (values (gethash code ,x->ucs-table)))))


;;; CP949 tables.

(defvar *cp949->ucs-hash* (make-hash-table))
(defvar *ucs->cp949-hash* (make-hash-table))
(X->UCS +cp949->unicode+ *cp949->ucs-hash* *ucs->cp949-hash*
        ucs->cp949 cp949->ucs)


;;; JOHAB tables.

(defvar *johab->ucs-hash* (make-hash-table))
(defvar *ucs->johab-hash* (make-hash-table))
(X->UCS +johab->unicode+ *johab->ucs-hash* *ucs->johab-hash*
        ucs->johab johab->ucs)



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
                 (loop with nchars fixnum = 0
                    with i fixnum = start
                    while (< i end) do
                      (let* ((octet (,getter seq i))
                             (next-i (+ i (if (>= octet #x80) 2 1))))
                        (declare (type ub8 octet) (fixnum next-i))
                        (cond ((> next-i end)
                               (decoding-error (vector octet) :cp949 seq i nil 'end-of-input-in-character)
                               (return (values (1+ nchars) end)))
                              (t
                               (setq nchars (1+ nchars)
                                     i next-i)
                               (when (and (plusp max) (= nchars max))
                                 (return (values nchars i))))))
                    finally (progn (assert (= i end))
                                   ;;(format t "code-point-counter n-chars=~a / i=~a.~%" nchars i)
                                   (return (values nchars i))))))


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
     (let ((u2 0))
       (declare (type ub8 u2))
       (loop for di fixnum from d-start
             for i fixnum from start below end
             for u1 of-type ub8 = (,getter src i) do
               (macrolet
                   ((consume-octet ()
                      `(let ((next-i (incf i)))
                         (if (= next-i end)
                             (return-from setter-block
                               (decoding-error nil :cp949 src i +repl+
                                               'end-of-input-in-character))
                             (,',getter src next-i))))
                    (handle-error (n &optional (c 'character-decoding-error))
                      `(decoding-error
                        (vector ,@(subseq '(u1 u2) 0 n))
                        :cp949 src (1+ (- i ,n)) +repl+ ',c))
                    (handle-error-if-icb (var n)
                      `(when (not (< #x7f ,var #xc0))
                         (decf i)
                         (return-from setter-block
                           (handle-error ,n invalid-utf8-continuation-byte)))))
                 (,setter
                  (block setter-block
                    (cond
                      ;; 2 octets
                      ((or (= u1 #x8e)
                           (< #xa0 u1 #xff))
                       (cp949->ucs (logior (f-ash u1 8)
                                           (consume-octet))))
                      ;; 1 octet
                      (t u1)))
                  dest di))
         finally (return (the fixnum (- di d-start)))))))


;;;EOF.
