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

(defvar edn--handlers (make-hash-table :test #'equal))

(defun edn--create-char (match)
  (cond
   ((string-prefix-p "\\u" match) (read (format "?%s" match))) ; unicode
   ((= (length match) 2) (string-to-char (substring match 1))) ; chars like \a
   (t (intern (substring match 1))))) ; chars like \newline

(defun edn--create-string (match)
  (with-temp-buffer
    (insert match)
    (goto-char (point-min))
    (let ((escaping nil)
          (escape-char ?\\)
          s)
      (while (not (eobp))
        (if (and (not escaping)
                 (eq (char-after) escape-char))
            (setq escaping t)
          (progn (when escaping
                   (setq escaping nil)
                   (when (looking-at "[tnr]")
                     (push escape-char s)))
                 (push (char-after) s)))
        (forward-char))
      (concat (nreverse s)))))

(defun edn--maybe-add-to-list ()
  (if (not discarded)
      (let ((v (pop peg-stack)))
        (push (cons v (pop peg-stack)) peg-stack))
    (setq discarded nil)
    ::dummy))

(defun edn--create-hash-table (key-vals)
  (unless (= (% (length key-vals) 2) 0)
    (error "A map requires an even number of forms!"))
  (let ((m (make-hash-table :test #'equal))
        val)
    (while key-vals
      (puthash (pop key-vals) (pop key-vals) m))
    m))

(cl-defstruct
    (edn-set
     (:type list)
     :named
     (:constructor nil)
     (:constructor edn--create-set (vals)))
  vals)

(defun edn--create-tagged-value (tag value)
  (-if-let (handler (gethash tag edn--handlers))
      (funcall handler value)
    (error "Don't know how to handle tag '%s'" tag)))

(defun edn--stringlike-to-string (stringlike)
  (cond ((stringp stringlike) stringlike)
        ((or (symbolp stringlike) (keywordp stringlike))
         (s-chop-prefix ":" (symbol-name stringlike)))
        (t (error "Can't convert '%s' to string" stringlike))))

;;;###autoload
(defun edn-parse (edn-string)
  "Parse one edn value from EDN-STRING."
  (let (discarded)
    (first
     (peg-parse-string
      ((form _ (opt (or elide value err)) _)
       (value (or string char bool integer float symbol keyword list vector map
                  set tagged-value))

       (char (substring "\\" (+ alphanum))
             `(c -- (edn--create-char c)))

       (bool (substring (or "true" "false"))
             `(bool -- (when (string-equal bool "true") t)))

       (symbol (substring (or slash symbol-with-prefix symbol-no-ns))
               (if terminating) `(symbol -- (intern symbol)))
       (additional-symbol-chars ["*+!-_?$%&=<>:#."])
       (symbol-constituent (or alphanum additional-symbol-chars))
       (symbol-start (or alpha ["*!_?$%&=<>."]
                         (and (or "-" "+") (or alpha additional-symbol-chars))))
       (slash "/")
       (symbol-with-prefix symbol-start (* symbol-constituent) slash
                           (+ symbol-constituent))
       (symbol-no-ns symbol-start (* symbol-constituent))

       (keyword (substring keyword-start
                           (or (and (* symbol-constituent) slash
                                    (+ symbol-constituent))
                               (+ symbol-constituent)))
                (if terminating) `(kw -- (intern kw)))
       (keyword-start ":" (or alphanum ["*+!-_?$%&=<>#."]))

       (string "\"" (substring string-content) "\""
               `(str -- (edn--create-string str)))
       (string-content (* (or "\\" (not "\"")) (any)))
       (string1 "\"" string-content "\"")

       (integer (substring integer1) (if terminating)
                `(i -- (string-to-number i)))
       (integer1 (or "+" "-" "")
                 (or (and [1-9] (* [0-9]))
                     [0-9]))

       (float (substring float1) (if terminating)
              `(f -- (string-to-number f)))

       (float1 (or (and integer1 frac exp)
                   (and integer1 frac)
                   (and integer1 exp)))

       (list "(" `(-- nil)
             (* _ (or elide value) _ `(-- (edn--maybe-add-to-list)) `(e _ -- e))
             ")" `(l -- (nreverse l)))

       (vector "[" `(-- nil)
               (* _ (or elide value) _ `(-- (edn--maybe-add-to-list)) `(e _ -- e))
               "]" `(l -- (vconcat (nreverse l))))

       (map "{" `(-- nil)
            (* _ (or elide value) _ `(-- (edn--maybe-add-to-list)) `(e _ -- e))
            "}" `(l -- (edn--create-hash-table (nreverse l))))

       (set "#{" `(-- nil)
            (* _ (or elide value) `(-- (edn--maybe-add-to-list)) `(e _ -- e))
            _ "}" `(l -- (edn-list-to-set (nreverse l))))

       (tagged-value "#" (substring alpha (or (and (* symbol-constituent) slash
                                                   (+ symbol-constituent))
                                              (* symbol-constituent)))
                     _ value _ `(tag val -- (edn--create-tagged-value tag val)))

       (frac "." (+ digit))
       (exp ex (+ digit))
       (ex (or "e" "E") (opt (or "-" "+")))

       (digit [0-9])
       (upper [A-Z])
       (lower [a-z])
       (alpha (or lower upper))
       (alphanum (or alpha digit))
       (terminating (or (set " \n\t()[]{}\";,") (eob)))
       (_ (* (or ws comment)))
       (comment ";" (* (not (or "\n" (eob))) (any)))
       (elide "#_" _ value `(-- (setq discarded t)) `(e _ _ -- e))
       (ws ["\t \n,"])

       (unsupported-bignum (substring (or float1 integer1) (or "N" "M"))
                           terminating
                           `(n -- (error "Unsupported bignum: %s" n)))
       (err (or unsupported-bignum
                (substring (+ (any)))) `(s -- (error "Invalid edn: '%s'" s))))
      edn-string))))

;;;###autoload
(defun edn-list-to-set (l)
  "Turn a list into `edn''s internal set representation"
  (edn--create-set l))

;;;###autoload
(defun edn-set-to-list (s)
  "Turn `edn''s internal set representation into a list"
  (set-vals set))

;;;###autoload
(defun edn-add-handler (tag handler)
  "Add a HANDLER function for TAG.

TAG is either a string, symbol or keyword. e.g. :my/cool-handler"
  (unless (or (stringp tag) (keywordp tag) (symbolp tag))
    (error "'%s' isn't a string, keyword or symbol!"))
  (unless (functionp handler)
    (error "'%s' isn't a valid handler function!"))
  (puthash (edn--stringlike-to-string tag) handler edn--handlers))

;;;###autoload
(defun edn-remove-handler (tag)
  "Remove a previously registered handler for TAG. "
  (puthash (puthash (edn--stringlike-to-string tag) nil edn--handlers)))

(provide 'edn)
;;; edn.el ends here
