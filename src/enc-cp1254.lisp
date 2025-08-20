;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enc-cp1254.lisp --- Implementation of the CP1254 character encoding.
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

(define-constant +cp1254-to-unicode+
    #(;; #x80
      #x20ac #xfffd #x201a #x0192 #x201e #x2026 #x2020 #x2021
      #x02c6 #x2030 #x0160 #x2039 #x0152 #xfffd #xfffd #xfffd
      ;; #x90
      #xfffd #x2018 #x2019 #x201c #x201d #x2022 #x2013 #x2014
      #x02dc #x2122 #x0161 #x203a #x0153 #xfffd #xfffd #x0178
      ;; #xa0
      #x00a0 #x00a1 #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
      #x00a8 #x00a9 #x00aa #x00ab #x00ac #x00ad #x00ae #x00af
      ;; #xb0
      #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
      #x00b8 #x00b9 #x00ba #x00bb #x00bc #x00bd #x00be #x00bf
      ;; #xc0
      #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
      #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
      ;; #xd0
      #x011e #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
      #x00d8 #x00d9 #x00da #x00db #x00dc #x0130 #x015e #x00df
      ;; #xe0
      #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
      #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
      ;; #xf0
      #x011f #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
      #x00f8 #x00f9 #x00fa #x00fb #x00fc #x0131 #x015f #x00ff)
  :test #'equalp)

(define-unibyte-decoder :cp1254 (octet)
  (if (< octet #x80)
      octet
      (svref +cp1254-to-unicode+ (the ub8 (- octet #x80)))))

(define-unibyte-encoder :cp1254 (code)
  (if (< code #x80)
      code
      (case code
        (#x20ac #x80)
        (#x201a #x82)
        (#x0192 #x83)
        (#x201e #x84)
        (#x2026 #x85)
        (#x2020 #x86)
        (#x2021 #x87)
        (#x02c6 #x88)
        (#x2030 #x89)
        (#x0160 #x8a)
        (#x2039 #x8b)
        (#x0152 #x8c)
        (#x2018 #x91)
        (#x2019 #x92)
        (#x201c #x93)
        (#x201d #x94)
        (#x2022 #x95)
        (#x2013 #x96)
        (#x2014 #x97)
        (#x02dc #x98)
        (#x2122 #x99)
        (#x0161 #x9a)
        (#x203a #x9b)
        (#x0153 #x9c)
        (#x0178 #x9f)
        (#x00a0 #xa0)
        (#x00a1 #xa1)
        (#x00a2 #xa2)
        (#x00a3 #xa3)
        (#x00a4 #xa4)
        (#x00a5 #xa5)
        (#x00a6 #xa6)
        (#x00a7 #xa7)
        (#x00a8 #xa8)
        (#x00a9 #xa9)
        (#x00aa #xaa)
        (#x00ab #xab)
        (#x00ac #xac)
        (#x00ad #xad)
        (#x00ae #xae)
        (#x00af #xaf)
        (#x00b0 #xb0)
        (#x00b1 #xb1)
        (#x00b2 #xb2)
        (#x00b3 #xb3)
        (#x00b4 #xb4)
        (#x00b5 #xb5)
        (#x00b6 #xb6)
        (#x00b7 #xb7)
        (#x00b8 #xb8)
        (#x00b9 #xb9)
        (#x00ba #xba)
        (#x00bb #xbb)
        (#x00bc #xbc)
        (#x00bd #xbd)
        (#x00be #xbe)
        (#x00bf #xbf)
        (#x00c0 #xc0)
        (#x00c1 #xc1)
        (#x00c2 #xc2)
        (#x00c3 #xc3)
        (#x00c4 #xc4)
        (#x00c5 #xc5)
        (#x00c6 #xc6)
        (#x00c7 #xc7)
        (#x00c8 #xc8)
        (#x00c9 #xc9)
        (#x00ca #xca)
        (#x00cb #xcb)
        (#x00cc #xcc)
        (#x00cd #xcd)
        (#x00ce #xce)
        (#x00cf #xcf)
        (#x011e #xd0)
        (#x00d1 #xd1)
        (#x00d2 #xd2)
        (#x00d3 #xd3)
        (#x00d4 #xd4)
        (#x00d5 #xd5)
        (#x00d6 #xd6)
        (#x00d7 #xd7)
        (#x00d8 #xd8)
        (#x00d9 #xd9)
        (#x00da #xda)
        (#x00db #xdb)
        (#x00dc #xdc)
        (#x0130 #xdd)
        (#x015e #xde)
        (#x00df #xdf)
        (#x00e0 #xe0)
        (#x00e1 #xe1)
        (#x00e2 #xe2)
        (#x00e3 #xe3)
        (#x00e4 #xe4)
        (#x00e5 #xe5)
        (#x00e6 #xe6)
        (#x00e7 #xe7)
        (#x00e8 #xe8)
        (#x00e9 #xe9)
        (#x00ea #xea)
        (#x00eb #xeb)
        (#x00ec #xec)
        (#x00ed #xed)
        (#x00ee #xee)
        (#x00ef #xef)
        (#x011f #xf0)
        (#x00f1 #xf1)
        (#x00f2 #xf2)
        (#x00f3 #xf3)
        (#x00f4 #xf4)
        (#x00f5 #xf5)
        (#x00f6 #xf6)
        (#x00f7 #xf7)
        (#x00f8 #xf8)
        (#x00f9 #xf9)
        (#x00fa #xfa)
        (#x00fb #xfb)
        (#x00fc #xfc)
        (#x0131 #xfd)
        (#x015f #xfe)
        (#x00ff #xff)
        (t (handle-error)))))

(define-character-encoding :cp1254
    "An 8-bit, fixed-width character Turkish encoding from Windows."
  :aliases '(:windows-1254)
  :literal-char-code-limit 256
  :codespace `((#x00 #x80)
               ,@(remove #xfffd (coerce +cp1254-to-unicode+ 'list))))
