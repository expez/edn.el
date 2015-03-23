;;; edn.el --- Support for reading and writing the edn data format from elisp

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/edn.el
;; Keywords: edn clojure
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.3") (emacs "24.4") (s "1.9.0") (dash "2.10.0"))

;; Copyright (c)  2015, Lars Andersen

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; Support for reading and writing the edn data format from elisp

;;; Code:

(require 's)
(require 'dash)
(require 'cl)
(require 'peg)

(defun edn--create-char (match)
  (cond
   ((string-prefix-p "\\u" match) (string-to-char match)) ; unicode chars
   ((= (length match) 2) (string-to-char (substring match 1))) ; chars like \a
   (t (intern (substring match 1))))) ; chars like \newline

(defun edn-parse (edn-string)
  (first
   (peg-parse-string
    ((form _ (opt (or elide char bool number symbol err)) _)

     (char (substring char1)
           `(c -- (edn--create-char c)))
     (char1 "\\" (+ alphanum) sep)

     (bool (substring bool1)
           `(bool -- (when (string-equal bool "true") t)))
     (bool1 (or "true" "false"))

     (symbol (substring symbol1)
             `(symbol -- (intern (string-trim symbol))))
     (symbol1 (or slash symbol-with-prefix symbol-no-ns) sep)
     (symbol-constituent (or alphanum ["*+!-_?$%&=<>:#."]))
     (symbol-start (or alpha ["*+!-_?$%&=<>."]))
     (slash "/")
     (symbol-with-prefix symbol-start (* symbol-constituent) slash
                         (+ symbol-constituent))
     (symbol-no-ns symbol-start (* symbol-constituent))

     (number (substring number1))
     (number1 (+ digit))

     (digit [0-9])
     (alpha [A-z])
     (alphanum (or alpha digit))
     (sep (or ws (bol) (eol)))
     (_ (* (or ws comment)))
     (comment (+ ";") (* (any)) (eol))
     (eol (or "\n" "\r\n" "\r"))
     (elide "#_" (* ws) (or char1 bool1 number1 symbol1) sep)
     (ws (or ["\t ,"] eol))
     (err (substring (+ (any))) `(s -- (error "Invalid edn: '%s'" s))))
    edn-string)))

(provide 'edn)
;;; edn.el ends here
