(require 'ert)
(require 'edn)

(ert-deftest empty-string ()
  :tags '(edn)
  (should (null (edn-parse ""))))

(ert-deftest spaces ()
  :tags '(edn)
  (should (null (edn-parse " ")))
  (should (null (edn-parse "   "))))

(ert-deftest tabs ()
  :tags '(edn)
  (should (null (edn-parse "	")))
  (should (null (edn-parse "		"))))

(ert-deftest commas ()
  :tags '(edn)
  (should (null (edn-parse ",")))
  (should (null (edn-parse ",,,,"))))

(ert-deftest only-whitespace ()
  :tags '(edn)
  (should (null (edn-parse "	  , ,
")))
  (should (null (edn-parse"
  ,, 	"))))

(ert-deftest symbols ()
  :tags '(edn)
  (should (equal 'foo (edn-parse "foo")))
  (should (equal 'foo\. (edn-parse "foo.")))
  (should (equal '%foo\. (edn-parse "%foo.")))
  (should (equal 'foo/bar (edn-parse "foo/bar")))
  (equal 'some\#sort\#of\#symbol (edn-parse "some#sort#of#symbol"))
  (equal 'truefalse (edn-parse "truefalse"))
  (equal 'true. (edn-parse "true."))
  (equal '/ (edn-parse "/"))
  (should (equal '.true (edn-parse ".true")))
  (should (equal 'some:sort:of:symbol (edn-parse "some:sort:of:symbol")))
  (equal 'foo-bar (edn-parse "foo-bar")))
