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

(defvar edn--readers (make-hash-table :test #'equal))
(defvar edn--writers (list))

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

(cl-defstruct
    (edn-inst
     (:type list)
     :named
     (:constructor nil)
     (:constructor edn--create-inst (high low)))
  high low)

(cl-defstruct
    (edn-uuid
     (:type list)
     :named
     (:constructor nil)
     (:constructor edn--create-uuid (uuid)))
  uuid)

(defun edn--create-tagged-value (tag value)
  (-if-let (reader (gethash tag edn--readers))
      (funcall reader value)
    (error "Don't know how to read tag '%s'" tag)))

(defun edn--stringlike-to-string (stringlike)
  (cond ((stringp stringlike) stringlike)
        ((or (symbolp stringlike) (keywordp stringlike))
         (s-chop-prefix ":" (symbol-name stringlike)))
        (t (error "Can't convert '%s' to string" stringlike))))

(defun edn--read ()
  (let (discarded)
    (first
     (peg-parse
      (form _ (opt (or elide value err)) _)
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
               (substring (+ (any)))) `(s -- (error "Invalid edn: '%s'" s)))))))

(defun edn--read-from-string (str)
  (with-current-buffer (get-buffer-create "*edn*")
    (delete-region (point-min) (point-max))
    (insert str)
    (goto-char (point-min))
    (edn--read)))

(defun edn--inst-reader (date-string)
  (edn-time-to-inst (date-to-time date-string)))

;;;###autoload
(defun edn-time-to-inst (time)
  "Turn a TIME, as defined in `time-date', into our internal
representation of an inst."
  (edn--create-inst (first time) (second time)))

;;;###autoload
(defun edn-inst-to-time (inst)
  "Turn our internal representation of an instant in time into a
  TIME from `time-date.'"
  (assert (edn-inst-p inst) nil "INST has to be of type `edn-inst'")
  (list (edn-inst-high inst) (edn-inst-low inst)))

;;;###autoload
(defun edn-string-to-uuid (s)
  "Create our internal representation of a uuid from a string, S,
containing a uuid."
  (edn--create-uuid s))

;;;###autoload
(defun edn-uuid-to-string (uuid)
  "Turn our internal representation of a UUID into a string."
  (assert (edn-uuid-p uuid) nil "UUID has to be of type `edn-uuid'")
  (edn-uuid-uuid uuid))

;;;###autoload
(defun edn-read (&optional source)
  "Read one edn value from SOURCE.

SOURCE is either a string of edn data or nil.  If no source is
given the next edn value will be read from POINT in the current
buffer."
  (cond
   ((null source) (edn--read))
   ((stringp source) (edn--read-from-string source))
   (t (error "Invalid source!"))))

;;;###autoload
(defun edn-list-to-set (l)
  "Turn a list into `edn''s internal set representation"
  (edn--create-set l))

;;;###autoload
(defun edn-set-to-list (s)
  "Turn `edn''s internal set representation into a list"
  (edn-set-vals s))

;;;###autoload
(defun edn-add-reader (tag reader)
  "Add a READER function for TAG.

TAG is either a string, symbol or keyword. e.g. :my/type"
  (unless (or (stringp tag) (keywordp tag) (symbolp tag))
    (error "'%s' isn't a string, keyword or symbol!" tag))
  (unless (functionp reader)
    (error "'%s' isn't a valid handler function!" reader))
  (puthash (edn--stringlike-to-string tag) reader edn--readers))

;;;###autoload
(defun edn-add-writer (pred writer &optional name)
  "Add a WRITER function for types satisfying PRED.

NAME can be specified to enable calling `edn-remove-writer`''"
  (unless (functionp writer)
    (error "'%s' isn't a valid writer function!" handler))
  (unless (functionp pred)
    (error "'%s' isn't a valid predicate function!" handler))
  (push (list :pred pred :writer writer :name name) edn--writers))

;;;###autoload
(defun edn-remove-reader (tag)
  "Remove a previously registered handler for TAG. "
  (puthash (puthash (edn--stringlike-to-string tag) nil edn--readers)))

;;;###autoload
(defun edn-remove-writer (name)
  "The remove the writer with name NAME."
  (-remove (lambda (writer-meta) (equal (plist-get writer-meta :name) name))
           edn--writers))

(defun edn--print-seq (open close values)
  (concat open (string-join (mapcar #'edn-print-string values) " ") close))

(defun edn--print-hash-map (m)
  (let ((keys (hash-table-keys m))
        (content ""))
    (concat "{"
            (dolist (k keys)
              (setq content (concat content " " (edn-print-string k) " "
                                    (edn-print-string (gethash k m)))))
            content
            "}")))

(cl-defun edn--custom-writer-for (datum)
  (dolist (writer edn--writers)
    (when (funcall (plist-get writer :pred ) datum)
      (cl-return (plist-get writer :writer )))))

(defun edn--uuid-writer (uuid)
  (concat "#uuid " (edn-uuid-to-string uuid)))

;;;###autoload
(defun edn-print-string (datum)
  (cond
   ((null datum) "nil")
   ((edn--custom-writer-for datum) (funcall (edn--custom-writer-for datum) datum))
   ((edn-set-p datum) (edn--print-seq "#{" "}" (edn-set-to-list datum)))
   ((listp datum) (edn--print-seq "(" ")" datum))
   ((vectorp datum) (edn--print-seq "[" "]" datum))
   ((hash-table-p datum) (edn--print-hash-map datum))
   ((stringp datum) (concat "\"" datum "\""))
   (t (format "%s" datum))))

(defun edn--inst-writer (inst)
  (format-time-string "#inst \"%Y-%m-%dT%H:%M:%S.52Z\""
                      (edn-inst-to-time inst)
                      :utc))

(edn-add-reader :inst #'edn--inst-reader)
(edn-add-writer #'edn-inst-p #'edn--inst-writer)
(edn-add-reader :uuid #'edn-string-to-uuid)
(edn-add-writer #'edn-uuid-p #'edn--uuid-writer)

(provide 'edn)
;;; edn.el ends here
