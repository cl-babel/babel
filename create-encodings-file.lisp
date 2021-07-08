(ql:quickload :drakma)
(ql:quickload :fare-csv)
(ql:quickload :split-sequence)

(defvar *csv-url* "https://www.iana.org/assignments/character-sets/character-sets-1.csv")
(defvar *lisp-file* "src/official-encoding-aliases.lisp")
(defvar *file-header*
";; Generate from Official Encoding name list.
;; See:
;; https://www.iana.org/assignments/character-sets/character-sets.xhtml
(in-package #:babel-encodings)

(defvar *official-encoding-aliases*
  `(")

(with-open-file (out *lisp-file*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (let ((csv-in (drakma:http-request *csv-url* :want-stream t)))
    (fare-csv:read-csv-line csv-in) ; skip header
    (format out "~A" *file-header*)
    (loop
       for row = (fare-csv:read-csv-line csv-in)
       while row
       for name = (nth 1 row)
       for aliases = (split-sequence:split-sequence #\Newline (nth 5 row))
       for whole-names = (cons name aliases)
       do
         (format out "~&~T~S~%" whole-names)
       finally
         (format out "))"))))
(quit)
