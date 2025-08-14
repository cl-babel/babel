;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1253.lisp --- Implementation of the CP1253 character encoding.
;;;
;;; Copyright (C) 2025, Wojciech S. Gac <wojciech.s.gac@gmail.com>
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

(define-character-encoding :cp1253
    "A 8-bit, fixed-width character encoding used by Windows for modern
Greek."
  :aliases '(:windows-1253)
  :literal-char-code-limit 256)

(define-constant +cp1253-to-unicode+
    #(#x20AC #x0000 #x201A #x0192 #x201E #x2026 #x2020 #x2021
      #x0000 #x2030 #x0000 #x2039 #x0000 #x0000 #x0000 #x0000
      #x0000 #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014
      #x0000 #x2122 #x0000 #x203A #x0000 #x0000 #x0000 #x0000
      #x00A0 #x0385 #x0386 #x00A3 #x00A4 #x00A5 #x00A6 #x00A7
      #x00A8 #x00A9 #x0000 #x00AB #x00AC #x00AD #x00AE #x2015
      #x00B0 #x00B1 #x00B2 #x00B3 #x0384 #x00B5 #x00B6 #x00B7
      #x0388 #x0389 #x038A #x00BB #x038C #x00BD #x038E #x038F
      #x0390 #x0391 #x0392 #x0393 #x0394 #x0395 #x0396 #x0397
      #x0398 #x0399 #x039A #x039B #x039C #x039D #x039E #x039F
      #x03A0 #x03A1 #x0000 #x03A3 #x03A4 #x03A5 #x03A6 #x03A7
      #x03A8 #x03A9 #x03AA #x03AB #x03AC #x03AD #x03AE #x03AF
      #x03B0 #x03B1 #x03B2 #x03B3 #x03B4 #x03B5 #x03B6 #x03B7
      #x03B8 #x03B9 #x03BA #x03BB #x03BC #x03BD #x03BE #x03BF
      #x03C0 #x03C1 #x03C2 #x03C3 #x03C4 #x03C5 #x03C6 #x03C7
      #x03C8 #x03C9 #x03CA #x03CB #x03CC #x03CD #x03CE #x0000)
  :test #'equalp)

(define-unibyte-decoder :cp1253 (octet)
  (if (and (>= octet #x80) (<= octet #xff))
      (svref +cp1253-to-unicode+
             (the ub8 (- octet #x80)))
      octet))

(define-constant +unicode-to-cp1253+
    (loop
      :with h := (make-hash-table)
      :for code :from #x80
      :for unicode :across +cp1253-to-unicode+
      :when unicode
        :do (setf (gethash unicode h) code)
      :finally (return h)))

(define-unibyte-encoder :cp1253 (code)
  (if (< code #x80)
      code
      (gethash code +unicode-to-cp1253+ #x00)))
