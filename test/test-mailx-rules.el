;;; test-mailx-rules.el --- Tests for mailx rules -*- lexical-binding: t; -*-

;;; Commentary:
;; Deterministic tests for rule matching and storage.

;;; Code:
(require 'ert)
(require 'mailx)
(require 'mailx-rules)

(ert-deftest mailx-rules-conditions ()
  (let ((msg '(:from ((:name "A Person" :email "a@example.org"))
               :cc ((:email "b@example.org")) :subject "Meeting"
               :tags ("retain" "tier:read") :list "list.example")))
    (should (mailx-eval-condition '(:from "example\\.org") msg))
    (should (mailx-eval-condition '(:to "b@example") msg))
    (should (mailx-eval-condition '(:tag "retain") msg))
    (should (mailx-eval-condition '(:has-unsubscribe) msg))
    (should (mailx-eval-condition '(:and (:subject "Meeting")
                                          (:not (:maildir "trash"))) msg))
    (should (mailx-eval-condition '(:or (:list "example") (:subject "No")) msg))
    (should (mailx-eval-condition '(:from "d@example")
                                  '(:from (("D" . "d@example.org")))))
    (should-error (mailx-eval-condition '(:or (:subject "Meeting") (:invalid)) msg))
    (should-error (mailx-eval-condition '(:or (:subject "Meeting") (:from "[")) msg))))

(ert-deftest mailx-rules-store-round-trip ()
  (let* ((root (make-temp-file "mailx-test" t))
         (rule (make-mailx-rule :id 'sample :match '(:subject "Hello")
                                :folder "/archive" :tier 'read :tags '("retain"))))
    (unwind-protect
        (progn
          (mailx-rules-save root (list rule))
          (should (equal (list rule) (mailx-rules-load root t)))
          (should (equal rule (mailx-classify-by-rules '(:subject "Hello")
                                                        (mailx-rules-load root)))))
      (delete-directory root t))))

(ert-deftest mailx-rules-cache-detects-external-edits ()
  (let* ((root (make-temp-file "mailx-test" t))
         (file (mailx-rules-file root))
         (rule (make-mailx-rule :id 'first :match '(:subject "Hi"))))
    (unwind-protect
        (progn
          (should-not (mailx-rules-load root))
          (mailx-rules-save root (list rule))
          (should (eq 'first (mailx-rule-id (car (mailx-rules-load root)))))
          (let ((mtime (file-attribute-modification-time (file-attributes file))))
            (with-temp-file file (insert "((:id other :match (:subject \"Hi\")))\n"))
            (set-file-times file mtime))
          (should (eq 'other (mailx-rule-id (car (mailx-rules-load root)))))
          (delete-file file)
          (should-not (mailx-rules-load root)))
      (delete-directory root t))))

(ert-deftest mailx-rules-preview-mu-result-and-empty ()
  (let ((rule (make-mailx-rule :id 'x :match '(:subject "match"))))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args)
                 (insert "(:subject \"a match\")\n(:subject \"other\")\n")
                 0)))
      (should (equal (mailx-test-rule "/tmp" rule)
                     '(:count 1 :subjects ("a match")))))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile destination _display &rest _args)
                 (with-temp-file (cadr destination)
                   (insert "no matches for search expression\n"))
                 2)))
      (should (equal (mailx-test-rule "/tmp" rule) '(:count 0 :subjects nil))))
    (cl-letf (((symbol-function 'process-file) (lambda (&rest _args) 1)))
      (should-error (mailx-test-rule "/tmp" rule)))))

(ert-deftest mailx-mu-find-rejects-ambiguous-status-two ()
  (seq-do
   (lambda (diagnostic)
     (cl-letf (((symbol-function 'process-file)
                (lambda (_program _infile destination _display &rest _args)
                  (with-temp-file (cadr destination) (insert diagnostic))
                  2)))
       (should-error (mailx--mu-find-output "/tmp" "query"))))
   '("" "database corrupt" "no matches for search expression\nerror: bad query"))
  (cl-letf (((symbol-function 'process-file)
             (lambda (_program _infile destination _display &rest _args)
               (with-temp-file (cadr destination)
                 (insert "no matches for search expression\n"))
               (insert "unexpected output")
               2)))
    (should-error (mailx--mu-find-output "/tmp" "query"))))

(ert-deftest mailx-rules-refile-skips-tag-only-rule ()
  (require 'mailx)
  (let ((message-directory "/tmp")
        (mailx-refile-rules nil)
        (rules (list (make-mailx-rule :id 'tag :match '(:subject "Hi")
                                     :tags '("retain"))
                     (make-mailx-rule :id 'folder :match '(:subject "Hi")
                                      :folder "/filed"))))
    (cl-letf (((symbol-function 'mailx-rules-load) (lambda (&rest _) rules)))
      (should (equal "/filed" (mailx-refile-folder '(:subject "Hi")))))))

(ert-deftest mailx-rules-unsafe-translation-fails-closed ()
  (should-error (mailx-condition-to-mu-query '(:subject ".*")))
  (should-error (mailx--condition-to-procmail '(:from "attacker\n* /dev/null")))
  (should-error (mailx--condition-to-fdm '(:from "attacker\" or match")))
  (should-error (mailx-export-procmail nil))
  (should-error (mailx-export-fdm nil)))

(ert-deftest mailx-rules-reject-unsafe-metadata ()
  (let ((root (make-temp-file "mailx-test" t)))
    (unwind-protect
        (progn
          (should-error (mailx-rules-save root (list (make-mailx-rule
                                                       :id 'x :match '(:subject "x")
                                                       :folder "/foo/../bar"))))
          (should-error (mailx-rules-save root (list (make-mailx-rule
                                                       :id 'x :match '(:subject "x")
                                                       :tags '("ok\nInjected: value"))))))
      (delete-directory root t))))

(ert-deftest mailx-rules-mu-query-compilation ()
  (should (equal "from:user@example.com"
                 (mailx-condition-to-mu-query '(:from "user@example.com"))))
  (should (equal "(from:alice AND subject:meeting)"
                 (mailx-condition-to-mu-query '(:and (:from "alice") (:subject "meeting")))))
  (should (equal "NOT (tag:retain)"
                 (mailx-condition-to-mu-query '(:not (:tag "retain")))))
  (should-error (mailx-condition-to-mu-query '(:subject ".*")))
  (should-error (mailx-condition-to-mu-query '(:has-unsubscribe)))
  (should-error (mailx-condition-to-mu-query '(:delivered-to "foo"))))

(ert-deftest mailx-rules-export-procmail-and-fdm ()
  (let ((rules (list (make-mailx-rule :id 'personal-sample
                                      :match '(:from "friend@example.com")
                                      :folder "/personal"
                                      :tier 'read
                                      :tags '("vip"))
                     (make-mailx-rule :id 'spam-sample
                                      :match '(:subject "viagra")
                                      :folder "/trash"
                                      :tier 'junk))))
    (let ((procmail (mailx-export-procmail rules)))
      (should (string-match-p "\\* \\^From:.*friend@example.com" procmail))
      (should (string-match-p "formail -I \"X-Label: tier:read vip\"" procmail))
      (should (string-match-p "\\$MAIL/personal/" procmail))
      (should (string-match-p "\\* \\^Subject:.*viagra" procmail))
      (should (string-match-p "/dev/null" procmail)))
    (let ((fdm (mailx-export-fdm rules)))
      (should (string-match-p "action \"act-personal-sample\"" fdm))
      (should (string-match-p "add-header \"X-Label\" \"tier:read vip\"" fdm))
      (should (string-match-p "maildir \"%h/mail/personal\"" fdm))
      (should (string-match-p "match \"\\^Subject:.*viagra\" in headers action drop" fdm)))))

(ert-deftest mailx-rules-export-all-writes-files ()
  (let* ((root (make-temp-file "mailx-export-" t))
         (rule (make-mailx-rule :id 'sample :match '(:from "a@b.com") :folder "/personal" :tier 'read)))
    (unwind-protect
        (progn
          (mailx-rules-save root (list rule))
          (let ((res (mailx-rules-export-all root)))
            (should (= (map-elt res :count) 1))
            (should (file-exists-p (map-elt res :procmail)))
            (should (file-exists-p (map-elt res :fdm)))
            (with-temp-buffer
              (insert-file-contents (map-elt res :procmail))
              (should (string-match-p "\\$MAIL/personal/" (buffer-string))))
            (with-temp-buffer
              (insert-file-contents (map-elt res :fdm))
              (should (string-match-p "%h/mail/personal" (buffer-string))))))
      (delete-directory root t))))

(ert-deftest mailx-rules-import-procmail ()
  (let* ((tmp-dir (make-temp-file "mailx-procmail-" t))
         (filter-file (expand-file-name "filter-test" tmp-dir)))
    (unwind-protect
        (progn
          (with-temp-file filter-file
            (insert ":0:\n* ^From:.*friend@example\\.com\n$MAIL/personal/\n\n"
                    ":0\n* ^Subject:.*spam\n/dev/null\n"))
          (let ((rules (mailx-import-procmail-file filter-file)))
            (should (= (length rules) 2))
            (let ((r1 (car rules))
                  (r2 (cadr rules)))
              (should (equal (mailx-rule-folder r1) "/personal"))
              (should (eq (mailx-rule-tier r1) 'read))
              (should (mailx-eval-condition (mailx-rule-match r1) '(:from "friend@example.com")))
              (should (null (mailx-rule-folder r2)))
              (should (eq (mailx-rule-tier r2) 'junk))
              (should (mailx-eval-condition (mailx-rule-match r2) '(:subject "spam"))))))
      (delete-directory tmp-dir t))))

(provide 'test-mailx-rules)
;;; test-mailx-rules.el ends here
