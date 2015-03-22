(require 'ert)
(require 'edn)

(ert-deftest returns-nil-on-empty-string ()
  (should (null (edn-parse ""))))

(ert-deftest returns-nil-on-spaces ()
  (should (null (edn-parse " ")))
  (should (null (edn-parse "   "))))

(ert-deftest returns-nil-on-tabs ()
  (should (null (edn-parse "	")))
  (should (null (edn-parse "		"))))

(ert-deftest returns-nil-on-commas ()
  (should (null (edn-parse ",")))
  (should (null (edn-parse ",,,,"))))

(ert-deftest returns-nil-on-only-whitespace ()
  (should (null (edn-parse "	  , ,
")))
  (should (null (edn-parse"
  ,, 	"))))

(ert-deftest parses-simple-symbols ()
  (should (equal 'foo (edn-parse "foo")))
  (should (equal 'foo\. (edn-parse "foo.")))
  (should (equal '%foo\. (edn-parse "%foo."))))

(ert-deftest parses-namespaced-symbols ()
  (should (equal 'foo/bar (edn-parse "foo/bar")))
  (should-error (edn-parse "/foobar")))
