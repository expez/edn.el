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

(defun edn-parse (edn-string)
  (first
   (peg-parse-string
    ((form (opt ws) (* (or number symbol)))
     (symbol (substring (or symbol-with-ns symbol-no-ns))
             `(symbol -- (intern symbol)))
     (non-numeric (or alpha ["*+!-_?$%&=<>:#."]))
     (symbol-with-ns non-numeric (* (or non-numeric alphanum)) slash
                     (+ (or non-numeric alphanum)))
     (symbol-no-ns non-numeric (+ (or alphanum non-numeric)))
     (slash "/")
     (alphanum (or alpha digit))
     (number (+ digit))
     (digit [0-9])
     (alpha [A-z])
     (ws (or "," "\n" "\t")))
    edn-string)))

(provide 'edn)
;;; edn.el ends here
