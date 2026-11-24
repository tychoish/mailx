;;; test-mailx-rules-store.el --- Tests for rule store errors, export and regex checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Load-error reporting, export robustness and regex heuristics.  Every test
;; uses a make-temp-file root, never a real mail tree.

;;; Code:
(require 'ert)
(require 'mailx)
(require 'mailx-rules)

(defun mailx-test--with-rules-text (text fn)
  "Write TEXT as the rules file of a temp root and call FN with the root."
  (let ((root (make-temp-file "mailx-test" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tools" root) t)
          (with-temp-file (mailx-rules-file root) (insert text))
          (funcall fn root))
      (delete-directory root t))))

(defun mailx-test--load-error-message (text)
  "Return the error message from loading rules TEXT, or nil."
  (mailx-test--with-rules-text
   text
   (lambda (root)
     (condition-case err
         (progn (mailx-rules-load root t) nil)
       (error (error-message-string err))))))

(ert-deftest mailx-rules-load-error-bad-regex ()
  (let ((msg (mailx-test--load-error-message
              "((:id bad-one :match (:subject \"[\") :folder \"/a\"))")))
    (should msg)
    (should (string-match-p "mailx-rules\\.eld" msg))
    (should (string-match-p "bad-one" msg))
    (should (string-match-p "index 0" msg))))

(ert-deftest mailx-rules-load-error-unknown-key ()
  (let ((msg (mailx-test--load-error-message
              "((:id ok :match (:subject \"x\") :folder \"/a\")
                (:id k2 :match (:subject \"y\") :priority 3))")))
    (should msg)
    (should (string-match-p "mailx-rules\\.eld" msg))
    (should (string-match-p "k2" msg))
    (should (string-match-p "index 1" msg))))

(ert-deftest mailx-rules-load-error-syntax ()
  (let ((msg (mailx-test--load-error-message "((:id a :match (:subject \"x\")")))
    (should msg)
    (should (string-match-p "mailx-rules\\.eld" msg))))

(ert-deftest mailx-rules-load-error-folder-no-slash ()
  (let ((msg (mailx-test--load-error-message
              "((:id nf :match (:subject \"x\") :folder \"archive\"))")))
    (should msg)
    (should (string-match-p "mailx-rules\\.eld" msg))
    (should (string-match-p "nf" msg))
    (should-not (string-match-p "#s(mailx-rule" msg))))

(ert-deftest mailx-rules-load-error-duplicate-ids ()
  (let ((msg (mailx-test--load-error-message
              "((:id dup :match (:subject \"x\") :folder \"/a\")
                (:id other :match (:subject \"z\") :folder \"/a\")
                (:id dup :match (:subject \"y\") :folder \"/b\"))")))
    (should msg)
    (should (string-match-p "mailx-rules\\.eld" msg))
    (should (string-match-p "dup" msg))
    (should (string-match-p "0" msg))
    (should (string-match-p "2" msg))))

(ert-deftest mailx-rules-load-error-trailing-data ()
  (let ((msg (mailx-test--load-error-message
              "((:id a :match (:subject \"x\") :folder \"/a\")) garbage")))
    (should msg)
    (should (string-match-p "mailx-rules\\.eld" msg))))

(ert-deftest mailx-rules-load-valid-with-trailing-whitespace-and-cache ()
  (let ((rule (make-mailx-rule :id 'sample :match '(:subject "Hello") :folder "/archive")))
    (mailx-test--with-rules-text
     "((:id sample :match (:subject \"Hello\") :folder \"/archive\" :source manual))\n\n  "
     (lambda (root)
       (should (equal (list rule) (mailx-rules-load root t)))
       (should (equal (list rule) (mailx-rules-load root)))))))

(provide 'test-mailx-rules-store)
;;; test-mailx-rules-store.el ends here
