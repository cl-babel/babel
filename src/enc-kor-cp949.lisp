;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-kor-cp949.lisp --- Korean CP949/UHC/EUC-KR encoding.
;;;

(in-package #:babel-encodings)


(defvar *cp949->ucs-hash* (make-hash-table))
(defvar *ucs->cp949-hash* (make-hash-table))


(loop for (cp949 uc) in +cp949->unicode+
   do (progn
        (setf (gethash cp949 *cp949->ucs-hash*) uc)
        (setf (gethash uc *ucs->cp949-hash*) cp949)))


(macrolet ((map-code-or-nil (m code (code-var) passthru?-code)
             (let ((%v (gensym))
                   (%found? (gensym)))
               `(if (let ((,code-var ,code)) ,passthru?-code)
                    ,code
                    (multiple-value-bind (,%v ,%found?)
                        (gethash ,code ,m)
                      (if ,%found?
                          ,%v
                          nil))))))
  ;;;
  (defun cp949->ucs (code)
    (declare (inline))
    (map-code-or-nil *cp949->ucs-hash* code
                     (c)
                     (< c #x80)))
  ;;;
  (defun ucs->cp949 (code)
    (declare (inline))
    (map-code-or-nil *ucs->cp949-hash* code
                     (c)
                     (< c #x80))))



(define-character-encoding :cp949
    "Korean CP949 encoding. An 8-bit, variable-length character
encoding. ASCII codes can be encoded in a single octet; characters
with larger code values can be encoded in 2 bytes."
  :max-units-per-char 2
  :literal-char-code-limit #x80
  :aliases '(:euckr :uhc))


;; Copied from GBK encoding.
(define-octet-counter :cp949 (getter type)  
  `(lambda (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (let ((noctets 0))
       (loop for i from start below end
          for u1 of-type code-point = (,getter seq i)
          do (cond ((< u1 #x80) (incf noctets))
                   (t (incf noctets 2)))
            (when (and (plusp max) (= noctets max))
              (return (values noctets i)))
          finally (return (values noctets i))))))

;; Copied from EUC-JP encoding.
(define-code-point-counter :cp949 (getter type)
  `(named-lambda cp949-code-point-counter (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (loop with nchars fixnum = 0
        with i fixnum = start
        while (< i end) do
          (let* ((octet (,getter seq i))
                 (next-i (+ i (cond ((or (< #xa0 octet #xff)
                                         (< #x80 octet #xa1)
                                         (< #xa0 octet #xc6)
                                         (= #xc6 octet)) 2)
                                    (t 1)))))
            (declare (type ub8 octet) (fixnum next-i))
            (cond ((> next-i end)
                   ;; Should we add restarts to this error, we'll have
                   ;; to figure out a way to communicate with the
                   ;; decoder since we probably want to do something
                   ;; about it right here when we have a chance to
                   ;; change the count or something.  (Like an
                   ;; alternative replacement character or perhaps the
                   ;; existence of this error so that the decoder
                   ;; doesn't have to check for it on every iteration
                   ;; like we do.)
                   ;;
                   ;; FIXME: The data for this error is not right.
                   (decoding-error (vector octet) :cp949 seq i
                                   nil 'end-of-input-in-character)
                   (return (values (1+ nchars) end)))
                  (t
                   (setq nchars (1+ nchars)
                         i next-i)
                   (when (and (plusp max) (= nchars max))
                     (return (values nchars i))))))
        finally (progn (assert (= i end))
                       (return (values nchars i))))))


(define-encoder :cp949 (getter src-type setter dest-type)
  `(named-lambda cp949-encoder (src start end dest d-start)
     (declare (type ,src-type src)
              (type ,dest-type dest)
              (fixnum start end d-start))
     (loop with di fixnum = d-start
        for i fixnum from start below end
        for code of-type code-point = (,getter src i)
        for cp949 of-type code-point
          = (ucs->cp949 code) do
          (macrolet ((set-octet (offset value)
                       `(,',setter ,value dest (the fixnum (+ di ,offset)))))
            (cond
              ;; Unmappables.
              ((null cp949)
               (encoding-error code :cp949 src start))
              ;; 2 octets
              ((> cp949 #x80)
               (set-octet 0 (f-logand #xff (f-ash cp949 -8)))
               (set-octet 1 (logand cp949 #xff))
               (incf di 2))
              ;; 1 octet
              (t
               (set-octet 0 cp949)
               (incf di))))
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
          ;; Note: CONSUME-OCTET doesn't check if I is being
          ;; incremented past END.  We're assuming that END has
          ;; been calculated with the CODE-POINT-POINTER above that
          ;; checks this.
            (macrolet
                ((consume-octet ()
                   `(let ((next-i (incf i)))
                      (if (= next-i end)
                          ;; FIXME: data for this error is incomplete.
                          ;; and signalling this error twice
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
                        (handle-error ,n invalid-utf8-continuation-byte))))
                 (->ucs-or-decoding-error (mapping-code
                                           (err-octets err-enc
                                                       err-buf err-pos
                                                       err-sub err-e))
                   (let ((%mapped (gensym)))
                     `(let ((,%mapped ,mapping-code))
                        (if (null ,%mapped)                            
                            (decoding-error
                             ,err-octets ,err-enc
                             ,err-buf ,err-pos ,err-sub ,err-e)
                            ,%mapped)))))
              (,setter
               (block setter-block
                 (cond
                   ;; 2 octets
                   ((or (< #xa0 u1 #xff)
                        (< #x80 u1 #xa1)
                        (< #xa0 u1 #xc6)
                        (= #xc6 u1))
                    (->ucs-or-decoding-error
                     (cp949->ucs (logior (f-ash u1 8)
                                         (consume-octet)))
                     ((vector u1) :cp949
                      src i +repl+ 'character-decoding-error)))
                   ;; 1 octet
                   (t
                    (->ucs-or-decoding-error
                     (cp949->ucs u1)
                     ((vector u1) :cp949
                      src (1- i) +repl+ 'character-decoding-error)))))
                 dest di))
          finally (return (the fixnum (- di d-start)))))))


