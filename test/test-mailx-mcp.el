;;; test-mailx-mcp.el --- Tests for mailx MCP inspection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests service registration and its read-only handlers without a live server.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'mailx-mcp)

(ert-deftest test-mailx-mcp-service-registration ()
  (let* ((mcpkit-registry nil)
         (service (mailx-mcp-register))
         (tools (mcpkit-service-tools service)))
    (should (map-elt tools "list_unclassified"))
    (should (map-elt tools "test_rule"))
    (should (map-elt tools "add_rule"))
    (should (map-elt tools "stage_triage_plan"))))

(ert-deftest test-mailx-mcp-allowlist-and-canonicalization ()
  (let ((root (make-temp-file "mailx-mcp-test-" t))
        (other (make-temp-file "mailx-mcp-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".mu" root))
          (make-directory (expand-file-name ".mu" other))
          (let ((mailx-mcp-allowed-roots (list root)))
            (should (equal (mailx-mcp--root (concat root "/."))
                           (file-name-as-directory (file-truename root))))
            (should-error (mailx-mcp--root other))
            (should-error (mailx-mcp--root "/ssh:host:/tmp/mail")))
          (let ((mailx-mcp-allowed-roots nil))
            (should-error (mailx-mcp--root root)))
          (delete-directory (expand-file-name ".mu" root))
          (make-symbolic-link (expand-file-name ".mu" other)
                              (expand-file-name ".mu" root))
          (let ((mailx-mcp-allowed-roots (list root)))
            (should-error (mailx-mcp--root root))))
      (delete-directory root t)
      (delete-directory other t))))

(ert-deftest test-mailx-mcp-list-messages ()
  (let ((root (make-temp-file "mailx-mcp-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".mu" root))
          (let ((mailx-mcp-allowed-roots (list root))
                (mailx-unclassified-query "tag:missing"))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (_program _in destination _display &rest arguments)
                         (should (member "--maxnum=3" arguments))
                         (should (member "tag:missing" arguments))
                         (with-current-buffer (car destination)
                           (insert "(:subject \"Hello\" :message-id \"id\")"))
                         0)))
              (should (equal (mailx-mcp--list-messages root 3)
                             [(:subject "Hello" :from "nil" :message_id "id" :maildir nil)])))))
      (delete-directory root t))))

(ert-deftest test-mailx-mcp-errors-are-not-empty-results ()
  (let ((root (make-temp-file "mailx-mcp-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".mu" root))
          (let ((mailx-mcp-allowed-roots (list root)))
            (should-error (mailx-mcp--list-messages root 101))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (_program _in destination _display &rest _arguments)
                         (with-temp-file (cadr destination) (insert "database corrupt"))
                         2)))
              (should-error (mailx-mcp--list-messages root 1)))
            (cl-letf (((symbol-function 'process-file)
                       (lambda (_program _in destination _display &rest _arguments)
                         (with-temp-file (cadr destination)
                           (insert "no matches for search expression\n"))
                         2)))
              (should (equal (mailx-mcp--list-messages root 1) [])))))
      (delete-directory root t))))

(ert-deftest test-mailx-mcp-add-rule-with-or-condition-survives-export ()
  (let ((root (make-temp-file "mailx-mcp-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".mu" root))
          (let ((mailx-mcp-allowed-roots (list root))
                (mailx-mcp-direct-rule-writes t))
            (let ((res (mailx-mcp--add-rule
                        root '(:id "either" :folder "/y"
                               :match (:or (:from "b@example") (:from "c@example"))))))
              (should (equal "ok" (plist-get res :status)))
              (should (plist-get res :skipped)))
            (should (equal 1 (length (mailx-rules-load root t))))
            (should (equal "ok" (plist-get (mailx-mcp--add-rule
                                            root '(:id "other" :folder "/z"
                                                   :match (:from "d@example")))
                                           :status)))
            (should (equal 2 (length (mailx-rules-load root t))))))
      (delete-directory root t))))

(ert-deftest test-mailx-mcp-add-rule-stages-a-plan-instead-of-writing ()
  (let ((root (make-temp-file "mailx-mcp-test-" t))
        (mailx-mcp-direct-rule-writes nil)
        (created nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".mu" root))
          (let ((mailx-mcp-allowed-roots (list root)))
            (cl-letf (((symbol-function 'mailx-bulk-ensure-domain) #'ignore)
                      ((symbol-function 'hitl-plan-create)
                       (lambda (&rest args) (setq created args) (hitl-plan--make :id "plan-7")))
                      ((symbol-function 'mailx-rules-save)
                       (lambda (&rest _) (error "must not save")))
                      ((symbol-function 'mailx-rules-export-all)
                       (lambda (&rest _) (error "must not export"))))
              (let ((res (mailx-mcp--add-rule
                          root '(:id "r1" :folder "/y" :match (:from "b@example")))))
                (should (equal "pending_review" (plist-get res :status)))
                (should (equal "plan-7" (plist-get res :plan_id)))
                (should (string-match-p "human" (plist-get res :message)))
                (should (eq 'mailx (plist-get created :domain)))
                (let* ((step (car (plist-get created :ops)))
                       (rule (car (cdr step))))
                  (should (eq 'promote-rule (car step)))
                  (should (eq 'r1 (mailx-rule-id rule))))
                (should (equal (file-name-as-directory root)
                               (file-name-as-directory
                                (map-elt (car (plist-get created :targets)) :root)))))
              ;; Validation still happens before staging.
              (should-error (mailx-mcp--add-rule
                             root '(:id "bad" :match (:from "\\(a+\\)+$"))))
              (should-error (mailx-mcp--add-rule root '(:match (:from "x")))))))
      (delete-directory root t))))

(ert-deftest test-mailx-mcp-staged-rule-plan-round-trips-and-applies ()
  (let ((root (make-temp-file "mailx-mcp-test-" t))
        (dir (make-temp-file "mailx-mcp-plans-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".mu" root))
          (let ((mailx-mcp-allowed-roots (list root))
                (hitl-plan-directory dir))
            (mailx-bulk-ensure-domain)
            (cl-letf (((symbol-function 'mailx-bulk-plan-directory) (lambda () dir)))
              (let* ((res (mailx-mcp--add-rule
                           root '(:id "r1" :folder "/y" :match (:from "b@example"))))
                     (plan (hitl-plan-load (plist-get res :plan_id) dir))
                     (step (car (hitl-plan-steps plan)))
                     (op (hitl-plan-step-op step))
                     (target (hitl-plan-step-target step)))
                (should (equal 0 (length (mailx-rules-load root t))))
                (should (mailx-rule-p (car (hitl-plan-op-args op))))
                (should (equal target (mailx-bulk--resolve-target target)))
                (let ((hitl-plan-current-target target))
                  (apply #'mailx-bulk--promote-rule-apply (hitl-plan-op-args op)))
                (should (equal '(r1) (seq-map #'mailx-rule-id (mailx-rules-load root t))))
                (let ((hitl-plan-current-target target))
                  (should-error (apply #'mailx-bulk--promote-rule-apply
                                       (hitl-plan-op-args op))))))))
      (delete-directory root t)
      (delete-directory dir t))))

(ert-deftest test-mailx-mcp-list-messages-rejects-empty-shared-query ()
  (let ((root (make-temp-file "mailx-mcp-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".mu" root))
          (let ((mailx-mcp-allowed-roots (list root))
                (mailx-unclassified-query "  "))
            (should-error (mailx-mcp--list-messages root 1))))
      (delete-directory root t))))

(provide 'test-mailx-mcp)
;;; test-mailx-mcp.el ends here
