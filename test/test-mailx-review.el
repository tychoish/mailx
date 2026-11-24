;;; test-mailx-review.el --- Tests for the mailx review buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Classification, plans and rule storage are stubbed; no mail is changed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mailx-review)

(defconst test-mailx-review--hit
  '(:subject "hi" :path "/fake/a" :message-id "a@example.org"
    :from (("A" . "a@example.org")) :maildir "/inbox"))

(defconst test-mailx-review--miss
  '(:subject "other" :path "/fake/b" :message-id "b@example.org"
    :from (("B" . "b@example.org")) :maildir "/inbox"))

(defun test-mailx-review--batch (&rest _)
  "Return a canned `mailx-classify-batch' result."
  (let ((rule (make-mailx-rule :id 'r1 :match '(:subject "hi") :folder "/archive" :tier 'read)))
    (list :results (list (list :path "/fake/a" :folder "/archive" :tier 'read :tags '("x")
                               :classification (list :rule rule)
                               :message test-mailx-review--hit))
          :unresolved (list test-mailx-review--miss))))

(defmacro test-mailx-review--with-buffer (&rest body)
  "Run BODY in a review buffer loaded from the canned batch result."
  (declare (indent 0))
  `(with-temp-buffer
     (cl-letf (((symbol-function 'mailx-classify-batch) #'test-mailx-review--batch))
       (mailx-review-mode)
       (setq mailx-review--root "/tmp/mail/")
       (mailx-review--load)
       (goto-char (point-min))
       ,@body)))

(ert-deftest mailx-review-load-lists-proposals-and-unresolved ()
  (test-mailx-review--with-buffer
    (should (= (length mailx-review--entries) 2))
    (should (= (length tabulated-list-entries) 2))
    (let ((proposed (car mailx-review--entries))
          (unresolved (cadr mailx-review--entries)))
      (should (equal (map-elt proposed :source) "rule:r1"))
      (should (eq (map-elt proposed :tier) 'read))
      (should-not (map-elt unresolved :tier))
      (should (equal (map-elt unresolved :folder) "/inbox"))
      (should (equal (map-elt unresolved :source) "none")))))

(ert-deftest mailx-review-load-uses-dry-run ()
  (let ((options nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'mailx-classify-batch)
                 (lambda (_root opts) (setq options opts) nil)))
        (mailx-review-mode)
        (setq mailx-review--root "/tmp/mail/")
        (mailx-review--load)))
    (should (map-elt options :dry-run))))

(ert-deftest mailx-review-edit-changes-only-the-pending-proposal ()
  (test-mailx-review--with-buffer
    (let ((entry (cadr mailx-review--entries)))
      (mailx-review-edit (list entry) :tier 'junk)
      (mailx-review-edit (list entry) :tags '("spam"))
      (mailx-review-edit (list entry) :folder "/trash")
      (should (eq (map-elt entry :tier) 'junk))
      (should (equal (map-elt entry :tags) '("spam")))
      (should (equal (map-elt entry :folder) "/trash"))
      (should (equal (map-elt entry :message) test-mailx-review--miss)))))

(ert-deftest mailx-review-edit-rejects-invalid-values ()
  (test-mailx-review--with-buffer
    (let ((entry (car mailx-review--entries)))
      (should-error (mailx-review-edit (list entry) :tier 'bogus) :type 'user-error)
      (should-error (mailx-review-edit (list entry) :folder "/a/../b") :type 'user-error)
      (should-error (mailx-review-edit (list entry) :folder "") :type 'user-error)
      (should-error (mailx-review-edit (list entry) :tags '("has space")))
      (should-error (mailx-review-edit (list entry) :tags '("tier:read")))
      (should (eq (map-elt entry :tier) 'read))
      (should (equal (map-elt entry :tags) '("x"))))))

(ert-deftest mailx-review-editing-an-accepted-entry-reopens-it ()
  (test-mailx-review--with-buffer
    (let ((entry (car mailx-review--entries)))
      (mailx-review-mark (list entry) 'accepted)
      (mailx-review-edit (list entry) :tier 'bulk)
      (should (eq (map-elt entry :status) 'pending)))))

(ert-deftest mailx-review-accept-requires-a-tier ()
  (test-mailx-review--with-buffer
    (let ((unresolved (cadr mailx-review--entries)))
      (should-error (mailx-review-mark (list unresolved) 'accepted) :type 'user-error)
      (should (eq (map-elt unresolved :status) 'pending))
      (mailx-review-edit (list unresolved) :tier 'junk)
      (mailx-review-mark (list unresolved) 'accepted)
      (should (eq (map-elt unresolved :status) 'accepted)))))

(ert-deftest mailx-review-accept-all-skips-incomplete-entries ()
  (test-mailx-review--with-buffer
    (mailx-review-accept-all)
    (should (equal (seq-map (lambda (entry) (map-elt entry :status)) mailx-review--entries)
                   '(accepted pending)))))

(ert-deftest mailx-review-commands-act-on-the-region ()
  (test-mailx-review--with-buffer
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (let ((transient-mark-mode t))
      (should (= (length (mailx-review--targets)) 2)))
    (deactivate-mark)
    (goto-char (point-min))
    (should (= (length (mailx-review--targets)) 1))))

(ert-deftest mailx-review-apply-groups-accepted-entries-into-one-plan ()
  (test-mailx-review--with-buffer
    (let ((entry (cadr mailx-review--entries))
          (ops nil)
          (reviewed nil))
      (mailx-review-edit (list entry) :tier 'read)
      (mailx-review-edit (list entry) :tags '("x"))
      (mailx-review-edit (list entry) :folder "/archive")
      (mailx-review-mark mailx-review--entries 'accepted)
      (cl-letf (((symbol-function 'mailx-bulk-ensure-domain) #'ignore)
                ((symbol-function 'hitl-plan-create) (lambda (&rest _) 'the-plan))
                ((symbol-function 'hitl-plan-add-op)
                 (lambda (_plan op targets) (push (cons op targets) ops)))
                ((symbol-function 'hitl-plan-review) (lambda (plan) (setq reviewed plan))))
        (mailx-review-apply))
      ;; Both entries share a classification, so one operation covers both.
      (should (= (length ops) 1))
      (should (equal (caar ops) '(classify read ("x") "/archive")))
      (should (equal (seq-map (lambda (target) (map-elt target :message-id)) (cdar ops))
                     '("a@example.org" "b@example.org")))
      (should (eq reviewed 'the-plan))
      (should (equal (seq-map (lambda (e) (map-elt e :status)) mailx-review--entries)
                     '(planned planned))))))

(ert-deftest mailx-review-apply-without-accepted-entries-does-nothing ()
  (test-mailx-review--with-buffer
    (cl-letf (((symbol-function 'hitl-plan-create)
               (lambda (&rest _) (error "must not create a plan"))))
      (should-error (mailx-review-apply) :type 'user-error))))

(ert-deftest mailx-review-planned-entries-cannot-be-edited-or-replanned ()
  (test-mailx-review--with-buffer
    (let ((entry (car mailx-review--entries)))
      (mailx-review-mark (list entry) 'accepted)
      (map-put! entry :status 'planned)
      (should-error (mailx-review-edit (list entry) :tier 'junk) :type 'user-error)
      (should-error (mailx-review-mark (list entry) 'skipped) :type 'user-error))))

(ert-deftest mailx-review-promote-saves-confirmed-rule ()
  (test-mailx-review--with-buffer
    (let ((saved nil))
      (cl-letf (((symbol-function 'mailx-rules-load) (lambda (&rest _) nil))
                ((symbol-function 'mailx-test-rule)
                 (lambda (&rest _) '(:count 3 :subjects ("hi"))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'mailx-rules-save)
                 (lambda (_root rules) (setq saved rules))))
        (let ((rule (mailx-review-promote-entry (car mailx-review--entries) "/tmp/mail/")))
          (should (mailx-rule-p rule))
          (should (equal saved (list rule)))
          (should (eq (mailx-rule-tier rule) 'read))
          (should (equal (mailx-rule-folder rule) "/archive"))
          (should (mailx-eval-condition (mailx-rule-match rule) test-mailx-review--hit)))))))

(ert-deftest mailx-review-promote-declined-saves-nothing ()
  (test-mailx-review--with-buffer
    (cl-letf (((symbol-function 'mailx-rules-load) (lambda (&rest _) nil))
              ((symbol-function 'mailx-test-rule) (lambda (&rest _) '(:count 1 :subjects nil)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
              ((symbol-function 'mailx-rules-save)
               (lambda (&rest _) (error "must not save"))))
      (should-not (mailx-review-promote-entry (car mailx-review--entries) "/tmp/mail/")))))

(ert-deftest mailx-review-promote-requires-a-tier-and-a-fresh-id ()
  (test-mailx-review--with-buffer
    (should-error (mailx-review-promote-entry (cadr mailx-review--entries) "/tmp/mail/")
                  :type 'user-error)
    (let ((existing nil))
      (cl-letf (((symbol-function 'mailx-rules-load) (lambda (&rest _) existing))
                ((symbol-function 'mailx-test-rule) (lambda (&rest _) '(:count 1 :subjects nil)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'mailx-rules-save) #'ignore))
        (setq existing (list (mailx-review-promote-entry (car mailx-review--entries) "/tmp/mail/")))
        (should-error (mailx-review-promote-entry (car mailx-review--entries) "/tmp/mail/")
                      :type 'user-error)))))

(ert-deftest mailx-review-folder-check-matches-rule-validation ()
  (test-mailx-review--with-buffer
    (let ((entry (car mailx-review--entries)))
      (should-error (mailx-review-edit (list entry) :folder "archive") :type 'user-error)
      (mailx-review-edit (list entry) :folder "/archive")
      (should (equal (map-elt entry :folder) "/archive")))))

(ert-deftest mailx-review-promote-invalid-folder-is-a-readable-error ()
  (test-mailx-review--with-buffer
    (let ((entry (car mailx-review--entries)))
      (map-put! entry :folder "archive")
      (cl-letf (((symbol-function 'mailx-rules-load) (lambda (&rest _) nil))
                ((symbol-function 'mailx-test-rule) (lambda (&rest _) '(:count 1 :subjects nil)))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "must not prompt")))
                ((symbol-function 'mailx-rules-save)
                 (lambda (&rest _) (error "must not save"))))
        (let ((err (should-error (mailx-review-promote-entry entry "/tmp/mail/")
                                 :type 'user-error)))
          (should-not (string-match-p "#s(" (error-message-string err)))
          (should (string-match-p "prop-a" (error-message-string err))))))))

(ert-deftest mailx-review-revert-confirms-before-discarding-decisions ()
  (test-mailx-review--with-buffer
    (mailx-review-mark (list (car mailx-review--entries)) 'accepted)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error (mailx-review--revert) :type 'user-error))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (mailx-review--revert)
      (should (eq (map-elt (car mailx-review--entries) :status) 'pending)))))

(ert-deftest mailx-review-interactive-edits-act-on-the-line-at-point ()
  (test-mailx-review--with-buffer
    (forward-line 1)
    (cl-letf (((symbol-function 'annotated-completing-read) (lambda (&rest _) "junk"))
              ((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (if (string-prefix-p "Folder" prompt) "/trash" "spam bulk"))))
      (mailx-review-set-tier)
      (mailx-review-set-folder)
      (mailx-review-set-tags))
    (let ((entry (cadr mailx-review--entries)))
      (should (eq (map-elt entry :tier) 'junk))
      (should (equal (map-elt entry :folder) "/trash"))
      (should (equal (map-elt entry :tags) '("spam" "bulk"))))
    ;; The entry above point is untouched.
    (should (eq (map-elt (car mailx-review--entries) :tier) 'read))))

(ert-deftest mailx-review-accept-and-skip-advance-point ()
  (test-mailx-review--with-buffer
    (mailx-review-accept)
    (should (eq (map-elt (car mailx-review--entries) :status) 'accepted))
    (should (equal (tabulated-list-get-id) "/fake/b"))
    (mailx-review-skip)
    (should (eq (map-elt (cadr mailx-review--entries) :status) 'skipped))))

(ert-deftest mailx-review-keymap-follows-keybinding-conventions ()
  (map-keymap-internal (lambda (event _binding)
                (when (characterp event)
                  (should-not (<= ?A event ?Z))
                  (should (zerop (logand event (lognot #x3fffff))))))
              mailx-review-mode-map))

(provide 'test-mailx-review)
;;; test-mailx-review.el ends here
