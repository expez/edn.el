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
