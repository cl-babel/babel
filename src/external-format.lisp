;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; external-format.lisp --- External format classes and functions.
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

(in-package #:babel)

(defvar *default-eol-style*
  #+(or win32 mswindows) :crlf
  #-(or win32 mswindows) :lf
  "The end-of-line style used by external formats if none is
explicitly given.  Depends on the OS the code is compiled on.")

(defclass external-format ()
  ((encoding :initarg :encoding :reader external-format-encoding)
   ;; one of :CR, :LF or :CRLF
   (eol-style :initarg :eol-style :reader external-format-eol-style
              :initform *default-eol-style*))
  (:documentation
   "An EXTERNAL-FORMAT consists in a combination of a Babel
CHARACTER-ENCODING and an end-of-line style."))

(defmethod print-object ((ef external-format) stream)
  (print-unreadable-object (ef stream :type t :identity t)
    (format stream "~A ~A"
            (enc-name (external-format-encoding ef))
            (external-format-eol-style ef))))

;;; This interface is still somewhat sketchy.  The rest of Babel
;;; doesn't really understand external formats, for instance.
(defun make-external-format (encoding &optional (eol-style *default-eol-style*))
  (make-instance 'external-format
                 :encoding (get-character-encoding encoding)
                 :eol-style eol-style))

(defun ensure-external-format (thing)
  (etypecase thing
    (external-format thing)
    (character-encoding (make-instance 'external-format :encoding thing))
    (symbol (make-external-format thing))))
