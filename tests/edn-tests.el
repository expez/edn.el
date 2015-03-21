(require 'ert)
(require 'edn)

(ert-deftest returns-nil-on-empty-string ()
  (should (null (edn-parse ""))))
