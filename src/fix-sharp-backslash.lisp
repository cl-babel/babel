;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; fix-sharp-backslash.lisp --- Alternative #\ dispatch code.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:babel-encodings)

;;; This is probably not portable but seems to work.
(defun signal-reader-error (stream thing)
  (error 'reader-error :stream stream
         :format-control "Unrecognized character name: u~A"
         :format-arguments (list thing)))

(defvar *original-sharp-backslash* nil)

#-allegro
(defun sharp-backslash (stream backslash numarg)
  (let ((1st-char (read-char stream)))
    (if (and (or (eql 1st-char #\u) (eql 1st-char #\U))
             (digit-char-p (peek-char nil stream) 16))
        (let* ((*read-base* 16)
               (token (read stream)))
          (if (typep token 'code-point)
              (code-char token)
              (signal-reader-error stream token)))
        (progn
          (unread-char 1st-char stream)
          (funcall *original-sharp-backslash* stream backslash numarg)))))

;;; Allegro's PEEK-CHAR seems broken on some situations, and the code
;;; above would generate an error about too many calls to UNREAD-CHAR.
;;; Then Allegro's original SHARP-BACKSLASH wants to UNREAD-CHAR
;;; twice, very weird.  This is the best workaround I could think of.
;;; It sucks.
#+allegro
(defun sharp-backslash (stream backslash numarg)
  (let* ((1st-char (read-char stream))
         (rest (excl::read-extended-token stream))
         (code (when (or (eql 1st-char #\u) (eql 1st-char #\U))
                 (ignore-errors (parse-integer rest :radix 16)))))
    (if code
        (code-char code)
        (with-input-from-string
            (s (concatenate 'string "#\\" (string 1st-char) rest))
          (read-char s)
          (read-char s)
          (funcall *original-sharp-backslash* s backslash numarg)))))

(when (null *original-sharp-backslash*)
  (setq *original-sharp-backslash* (get-dispatch-macro-character #\# #\\)))

(set-dispatch-macro-character #\# #\\ #'sharp-backslash)
