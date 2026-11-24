;;; test-mailx-classify.el --- Mail classification tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for deterministic classification and local Maildir rewriting.

;;; Code:
(require 'ert)
(require 'mailx-classify)

(ert-deftest mailx-classify-stage1-priority ()
  (let ((rule (make-mailx-rule :id 'test :match '(:subject "hello") :tier 'read)))
    (should (eq 'read (map-elt (mailx-classify-stage1
                                '(:from "a@example.org" :subject "hello" :list "list")
                                (list rule) '("a@example.org") '("a@example.org")) :tier)))
    (should (eq 'junk (map-elt (mailx-classify-stage1
                                '(:from "a@example.org") nil '("a@example.org")
                                '("a@example.org")) :tier)))
    (should (eq 'reply (map-elt (mailx-classify-stage1
                                 '(:from "A@EXAMPLE.ORG") nil '("a@example.org")) :tier)))
    (should (eq 'bulk (map-elt (mailx-classify-stage1 '(:list "list") nil) :tier)))
    (should-not (mailx-classify-stage1 '(:subject "hello") nil))))

(ert-deftest mailx-stamp-and-move-preserves-bytes-and-mtime ()
  (let* ((root (make-temp-file "mailx-test-" t))
         (source (expand-file-name "in/new/abc" root))
         (mtime (seconds-to-time 1000000000)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (seq-do (lambda (name) (make-directory (expand-file-name name root) t))
                  '("out/new" "out/cur" "out/tmp"))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert "From: a\r\nX-Label: retain, tier:read\r\n folded\r\nSubject: hi\r\n\r\nbody\377")
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max) source nil 'silent)))
          (set-file-times source mtime)
          (let ((target (mailx-stamp-and-move source root "/out" 'reply '("retain" "tag") 'exclusive-access)))
            (should-not (file-exists-p source))
            (should (equal mtime (file-attribute-modification-time (file-attributes target))))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally target)
              (should (equal (buffer-string)
                             "X-Label: retain folded tier:reply tag\r\nFrom: a\r\nSubject: hi\r\n\r\nbody\377")))))
      (delete-directory root t))))

(ert-deftest mailx-stamp-and-move-rejects-invalid-input ()
  (let* ((root (make-temp-file "mailx-test-" t))
         (source (expand-file-name "in/new/abc" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (with-temp-file source (insert "Subject: hi\n\nbody"))
          (should-error (mailx-stamp-and-move source root "../elsewhere" 'read nil 'exclusive-access))
          (should-error (mailx-stamp-and-move source root "out" 'invalid nil 'exclusive-access))
          (should (file-exists-p source)))
      (delete-directory root t))))

(ert-deftest mailx-classify-ignores-sender-display-name ()
  (should-not (mailx-classify-stage1
               '(:from ((:name "Trusted Person" :email "bad@example.org")))
               nil '("trusted person")))
  (should (eq 'reply (map-elt
                       (mailx-classify-stage1
                        '(:from ((:name "Trusted Person" :email "good@example.org")))
                        nil '("good@example.org")) :tier))))

(ert-deftest mailx-stamp-requires-coordination-and-rejects-symlinks ()
  (let* ((root (make-temp-file "mailx-test-" t))
         (source (expand-file-name "in/new/abc" root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source) t)
          (seq-do (lambda (name) (make-directory (expand-file-name name root) t))
                  '("real/new" "real/cur" "real/tmp"))
          (make-symbolic-link (expand-file-name "real" root)
                              (expand-file-name "link" root))
          (with-temp-file source (insert "Subject: hi\n\nbody"))
          (should-error (mailx-stamp-and-move source root "real" 'read))
          (should-error (mailx-stamp-and-move source root "link" 'read nil
                                              'exclusive-access))
          (should (file-exists-p source)))
      (delete-directory root t))))

(ert-deftest mailx-correspondents-and-thread-use-real-mu ()
  (skip-unless (executable-find "mu"))
  (let* ((root (make-temp-file "mailx-mu-" t))
         (home (expand-file-name ".mu" root))
         (file (expand-file-name "sent/cur/message:2,S" root)))
    (unwind-protect
        (progn
          (seq-do (lambda (name) (make-directory (expand-file-name name root) t))
                  '("sent/cur" "sent/new" "sent/tmp"))
          (with-temp-file file
            (insert "From: Me <me@example.org>\nTo: You <you@example.org>\n"
                    "Cc: Other <other@example.org>\nSubject: Example\n"
                    "Date: Sun, 15 Nov 2026 12:00:00 +0000\n"
                    "Message-ID: <example@example.org>\n"
                    "X-Label: tier:read keep\n\nbody\n"))
          (should (zerop (process-file "mu" nil nil nil "init" "--muhome" home
                                       "--maildir" root "--my-address" "me@example.org")))
          (should (zerop (process-file "mu" nil nil nil "index" "--muhome" home)))
          (should (equal '("other@example.org" "you@example.org")
                         (mailx-refresh-correspondents root "maildir:/sent")))
          (should (equal '("other@example.org" "you@example.org")
                         (with-temp-buffer
                           (insert-file-contents (expand-file-name "tools/mailx-correspondents.eld" root))
                           (read (current-buffer)))))
          (let ((result (mailx-thread-parent-classification
                         root '(:in-reply-to "<example@example.org>") "maildir:/sent")))
            (should (equal "/sent" (map-elt result :folder)))
            (should (eq 'reply (map-elt result :tier)))
            (should (equal '("keep") (map-elt result :tags))))
          (let ((result (mailx-thread-parent-classification
                         root '(:references "<absent@example.org> <example@example.org>"))))
            (should (eq 'read (map-elt result :tier)))
            (should (eq 'read (map-elt (mailx-classify-stage1 nil nil nil nil result) :tier))))
          (should-error (mailx-refresh-correspondents root "")))
      (delete-directory root t))))

(ert-deftest mailx-correspondents-load-validates ()
  (let* ((root (make-temp-file "mailx-cache-" t))
         (file (expand-file-name "tools/mailx-correspondents.eld" root)))
    (unwind-protect
        (progn
          (should-not (mailx-load-correspondents root))
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert "(\"a@example.org\")\n"))
          (should (equal '("a@example.org") (mailx-load-correspondents root)))
          (with-temp-file file (insert "(\"UPPER@example.org\")"))
          (should-error (mailx-load-correspondents root)))
      (delete-directory root t))))

(ert-deftest mailx-stage2-validates-output-and-only-sends-headers ()
  (let ((mailx-statistical-classifier-function
         (lambda (headers)
           (should-not (map-elt headers :body))
           '(:tier read :tags ("retain")))))
    (should (equal '(:tier read :tags ("retain"))
                   (mailx-classify-stage2 '(:subject "Hi" :body "secret")))))
  (let ((mailx-statistical-classifier-function (lambda (_) '(:tier bad))))
    (should-error (mailx-classify-stage2 nil))))

(ert-deftest mailx-stage3-is-header-only-and-validates-json ()
  (let ((gptel-use-tools t) (gptel-tools '(dangerous)) prompt result response)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (text &rest args)
                 (setq prompt text)
                 (should-not gptel-use-tools)
                 (should-not gptel-tools)
                 (should (eq gptel-backend 'trusted))
                 (funcall (map-elt args :callback) response nil))))
      (setq response "{\"tier\":\"bulk\",\"tags\":[\"list\"]}")
      (mailx-classify-stage3-gptel '(:subject "Hi" :body "hidden")
                                   (lambda (value) (setq result value)) 'trusted)
      (should-not (string-match-p "hidden" prompt))
      (should (equal '(:tier bulk :tags ("list")) result))
      (setq response "{\"tier\":\"bogus\"}")
      (mailx-classify-stage3-gptel nil (lambda (value) (setq result value)) 'trusted)
      (should-error (mailx-classify-stage3-gptel nil #'ignore nil))
      (should (map-elt result :error)))))

(ert-deftest mailx-stage2-command-sends-only-header-data ()
  (skip-unless (executable-find "python3"))
  (let ((mailx-statistical-classifier-function nil)
        (mailx-statistical-classifier-command
         '("python3" "-c" "import sys; s=sys.stdin.read(); print('{\"tier\":\"read\",\"tags\":[]}' if 'secret-body' not in s and 'subject' in s else 'bad')")))
    (should (equal '(:tier read :tags nil)
                   (mailx-classify-stage2 '(:subject "subject" :body "secret-body"))))))

(ert-deftest mailx-classify-message-returns-first-result ()
  (let ((rule (make-mailx-rule :id 'first :match '(:subject "hello") :tier 'reply))
        (mailx-statistical-classifier-function (lambda (_) (error "Unexpected stage 2")))
        result)
    (mailx-classify-message '(:subject "hello") (list rule)
                            (lambda (value) (setq result value)))
    (should (eq 'reply (map-elt result :tier))))
  (let ((mailx-statistical-classifier-function (lambda (_) '(:tier cursory))) result)
    (mailx-classify-message '(:subject "hello") nil (lambda (value) (setq result value)))
    (should (equal '(:tier cursory :tags nil) result))))

(ert-deftest mailx-classify-receipt-detection ()
  (let ((msg '(:from "orders@store.example.com" :subject "Your order confirmation #12345")))
    (let ((res (mailx-classify-stage1 msg nil)))
      (should (eq 'cursory (map-elt res :tier)))
      (should (member "receipt" (map-elt res :tags)))
      (should (member "retain" (map-elt res :tags))))))

(ert-deftest mailx-classify-propose-rule-derives-match ()
  (let* ((msg '(:from ((:name "Vendor" :email "billing@acme.example.com"))
                :subject "Monthly Invoice"))
         (classification '(:folder "/banking" :tier cursory :tags ("receipt")))
         (rule (mailx-classify-propose-rule msg classification)))
    (should (mailx-rule-p rule))
    (should (equal (mailx-rule-folder rule) "/banking"))
    (should (eq (mailx-rule-tier rule) 'cursory))
    (should (equal (mailx-rule-tags rule) '("receipt")))
    (should (eq (mailx-rule-source rule) 'proposal))
    (should (mailx-eval-condition (mailx-rule-match rule) msg))))

(ert-deftest mailx-classify-batch-processes-messages ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (rule (make-mailx-rule :id 'r1 :match '(:subject "hi") :folder "/archive" :tier 'read))
         (message '(:subject "hi" :path "/fake/path" :maildir "/inbox")))
    (unwind-protect
        (progn
          (mailx-rules-save root (list rule))
          (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) (list message))))
            (let ((res (mailx-classify-batch root '(:dry-run t))))
              (should (= (map-elt res :processed) 1))
              (should (= (map-elt res :classified) 1))
              (should (= (map-elt res :moved) 0)))))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-reports-move-errors ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (path (expand-file-name "msg" root))
         (rule (make-mailx-rule :id 'r1 :match '(:subject "hi") :folder "/archive" :tier 'read))
         (message (list :subject "hi" :path path :maildir "/inbox")))
    (unwind-protect
        (progn
          (write-region "" nil path)
          (mailx-rules-save root (list rule))
          (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) (list message))))
            (let ((res (mailx-classify-batch root '(:dry-run nil))))
              (should (= (map-elt res :moved) 0))
              (should (= (length (map-elt res :errors)) 1))
              (should (equal (car (car (map-elt res :errors))) path))
              (should (string-match-p "exclusive access"
                                      (cdr (car (map-elt res :errors))))))))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-flags-truncation-at-the-limit ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (messages (seq-map (lambda (n) (list :subject (format "m%d" n) :path "/none" :maildir "/inbox"))
                            '(1 2 3))))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find)
                   (lambda (_root _query &optional limit) (seq-take messages limit))))
          (should (map-elt (mailx-classify-batch root '(:dry-run t :limit 3)) :truncated))
          (should (map-elt (mailx-classify-batch root '(:dry-run t :limit 2)) :truncated))
          (should-not (map-elt (mailx-classify-batch root '(:dry-run t :limit 4)) :truncated)))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-refreshes-stale-correspondents ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (mailx-sent-query "from:me")
         (refreshed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) nil))
                  ((symbol-function 'mailx-refresh-correspondents)
                   (lambda (_root query) (setq refreshed query) nil)))
          (let ((res (mailx-classify-batch root '(:dry-run t))))
            (should (equal refreshed "from:me"))
            (should (eq (map-elt res :correspondents) 'refreshed)))
          ;; A fresh cache is left alone.
          (setq refreshed nil)
          (let ((file (expand-file-name mailx-correspondents-filename root)))
            (make-directory (file-name-directory file) t)
            (write-region "nil\n" nil file nil 'silent))
          (let ((res (mailx-classify-batch root '(:dry-run t))))
            (should-not refreshed)
            (should (eq (map-elt res :correspondents) 'fresh))))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-notes-skipped-correspondent-refresh ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (mailx-sent-query nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) nil)))
          (should (eq (map-elt (mailx-classify-batch root '(:dry-run t)) :correspondents)
                      'skipped-no-sent-query)))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-reports-correspondent-refresh-failure ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (mailx-sent-query "from:me"))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) nil))
                  ((symbol-function 'mailx-refresh-correspondents)
                   (lambda (&rest _) (error "mu exploded"))))
          (should (equal (map-elt (mailx-classify-batch root '(:dry-run t)) :correspondents)
                         '(error . "mu exploded"))))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-returns-messages-and-unresolved ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (rule (make-mailx-rule :id 'r1 :match '(:subject "hi") :folder "/archive" :tier 'read))
         (hit '(:subject "hi" :path "/fake/a" :maildir "/inbox"))
         (miss '(:subject "other" :path "/fake/b" :maildir "/inbox")))
    (unwind-protect
        (progn
          (mailx-rules-save root (list rule))
          (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) (list hit miss))))
            (let ((res (mailx-classify-batch root '(:dry-run t))))
              (should (equal (map-elt (car (map-elt res :results)) :message) hit))
              (should (equal (map-elt res :unresolved) (list miss))))))
      (delete-directory root t))))

(ert-deftest mailx-classify-stage1-ignores-tierless-parent ()
  (let ((parent '(:folder "/INBOX" :tier nil :tags ("x"))))
    (should (eq 'reply (map-elt (mailx-classify-stage1
                                 '(:from "a@example.org") nil '("a@example.org") nil parent)
                                :tier)))
    (should-not (mailx-classify-stage1 '(:subject "hello") nil nil nil parent))
    (should (eq 'read (map-elt (mailx-classify-stage1
                                '(:subject "hello") nil nil nil
                                '(:folder "/INBOX" :tier read))
                               :tier)))))

(ert-deftest mailx-classify-batch-tierless-result-is-unresolved ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (message '(:subject "hi" :path "/fake/a" :maildir "/inbox")))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) (list message)))
                  ((symbol-function 'mailx-classify-message)
                   (lambda (_m _r callback &optional _o)
                     (funcall callback '(:folder "/x" :tier nil)))))
          (let ((res (mailx-classify-batch root '(:dry-run t))))
            (should (= (map-elt res :classified) 0))
            (should-not (map-elt res :results))
            (should (equal (map-elt res :unresolved) (list message)))))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-uses-shared-unclassified-query ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (mailx-unclassified-query "maildir:/staging")
         (seen nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find)
                   (lambda (_root query &rest _) (setq seen query) nil)))
          (mailx-classify-batch root '(:dry-run t))
          (should (equal seen "maildir:/staging"))
          (mailx-classify-batch root '(:dry-run t :query "explicit"))
          (should (equal seen "explicit")))
      (delete-directory root t))))

(ert-deftest mailx-unclassified-query-old-names-are-aliases ()
  (should (eq 'mailx-unclassified-query (indirect-variable 'mailx-views-unclassified-query)))
  (should (eq 'mailx-unclassified-query (indirect-variable 'mailx-mcp-unclassified-query)))
  (let ((mailx-views-unclassified-query "q1"))
    (should (equal mailx-unclassified-query "q1")))
  (let ((mailx-mcp-unclassified-query "q2"))
    (should (equal mailx-unclassified-query "q2"))))

(ert-deftest mailx-classify-batch-loads-correspondents-once ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (loads 0)
         (messages (seq-map (lambda (n) (list :subject (format "m%d" n) :path "/fake" :maildir "/i"))
                            (number-sequence 1 5))))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) messages))
                  ((symbol-function 'mailx-load-correspondents)
                   (lambda (_root) (setq loads (1+ loads)) nil))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (mailx-classify-batch root '(:dry-run t))
          (should (= loads 1)))
      (delete-directory root t))))

(ert-deftest mailx-classify-message-correspondents-option ()
  (let ((loads 0)
        (root (make-temp-file "mailx-msg-" t))
        (res :unset))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx-load-correspondents)
                   (lambda (_root) (setq loads (1+ loads)) '("a@example.org")))
                  ((symbol-function 'mailx--mu-find) (lambda (&rest _) nil)))
          (mailx-classify-message '(:from "a@example.org") nil
                                  (lambda (r) (setq res r))
                                  (list :root root :correspondents '("a@example.org")))
          (should (= loads 0))
          (should (eq 'reply (map-elt res :tier)))
          (mailx-classify-message '(:from "a@example.org") nil
                                  (lambda (r) (setq res r))
                                  (list :root root))
          (should (= loads 1))
          (should (eq 'reply (map-elt res :tier))))
      (delete-directory root t))))

(ert-deftest mailx-classify-batch-reports-progress ()
  (let* ((root (make-temp-file "mailx-batch-" t))
         (logged nil)
         (messages (seq-map (lambda (n) (list :subject (format "m%d" n) :path "/fake" :maildir "/i"))
                            (number-sequence 1 12))))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx--mu-find) (lambda (&rest _) messages))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) logged))))
          (mailx-classify-batch root '(:dry-run t))
          (should (member "mailx: classifying 10/12" logged))
          (should-not (member "mailx: classifying 5/12" logged))
          (should (seq-find (lambda (m) (string-match-p "mailx: classified 0 of 12" m)) logged)))
      (delete-directory root t))))

(defun test-mailx-classify--maildir-fixture (root name)
  "Create synthetic Maildir folders under ROOT and a source message NAME."
  (let ((source (expand-file-name (concat "in/cur/" name) root)))
    (seq-do (lambda (dir) (make-directory (expand-file-name dir root) t))
            '("in/cur" "out/new" "out/cur" "out/tmp"))
    (write-region "From: a\nSubject: hi\n\nbody\n" nil source nil 'silent)
    source))

(ert-deftest mailx-stamp-and-move-keeps-base-name-and-flags ()
  (let* ((root (make-temp-file "mailx-test-" t))
         (source (test-mailx-classify--maildir-fixture root "1700.abc.host:2,RS")))
    (unwind-protect
        (let ((target (mailx-stamp-and-move source root "/out" 'read nil 'exclusive-access)))
          (should (equal (file-name-nondirectory target) "1700.abc.host:2,RS"))
          (should (equal (file-name-directory target)
                         (file-name-as-directory
                          (expand-file-name "out/cur" (file-truename root)))))
          (should-not (file-exists-p source))
          (should-not (directory-files (expand-file-name "out/tmp" root) nil "\\`[^.]")))
      (delete-directory root t))))

(ert-deftest mailx-stamp-and-move-collision-does-not-overwrite ()
  (let* ((root (make-temp-file "mailx-test-" t))
         (source (test-mailx-classify--maildir-fixture root "1700.abc.host:2,S"))
         (existing (expand-file-name "out/cur/1700.abc.host:2,S" root)))
    (unwind-protect
        (progn
          (write-region "precious" nil existing nil 'silent)
          (let ((target (mailx-stamp-and-move source root "/out" 'read nil 'exclusive-access)))
            (should-not (equal (file-name-nondirectory target) "1700.abc.host:2,S"))
            (should (string-prefix-p "1700.abc.host" (file-name-nondirectory target)))
            (should (string-suffix-p ":2,S" target))
            (should (file-exists-p target))
            (should (equal "precious"
                           (with-temp-buffer (insert-file-contents existing) (buffer-string))))))
      (delete-directory root t))))

(provide 'test-mailx-classify)
;;; test-mailx-classify.el ends here
