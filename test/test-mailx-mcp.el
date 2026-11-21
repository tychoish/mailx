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
                (mailx-mcp-unclassified-query "tag:missing"))
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

(provide 'test-mailx-mcp)
;;; test-mailx-mcp.el ends here
