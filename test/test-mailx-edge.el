;;; test-mailx-edge.el --- Mail edge-case tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Edge cases for refile rules, unsubscribe parsing, display values and the
;; lazily installed cmdr keybindings.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'mailx)

(dolist (dir (append '("/usr/share/emacs/site-lisp/mu4e")
                     (file-expand-wildcards "/usr/share/emacs/site-lisp/elpa-src/mu4e*")
                     (file-expand-wildcards "/usr/share/emacs/site-lisp/elpa/mu4e*")))
  (when (and (file-directory-p dir) (not (member dir load-path)))
    (add-to-list 'load-path dir)))

(require 'mu4e)
(require 'mu4e-compose)
(require 'mu4e-headers)

(defvar org-capture-templates nil)

(ert-deftest mailx-edge/refile-folder-defaults-to-archive ()
  (let ((mailx-refile-rules nil))
    (should (equal "/archive" (mailx-refile-folder '(:subject "x"))))))

(ert-deftest mailx-edge/refile-rules-first-match-wins-and-nil-falls-through ()
  (let ((mailx-refile-rules nil))
    (mailx-add-refile-rule (lambda (_) nil))
    (mailx-add-refile-rule (lambda (_) "/a"))
    (mailx-add-refile-rule (lambda (_) "/b"))
    (should (equal "/a" (mailx-refile-folder nil)))))

(ert-deftest mailx-edge/add-refile-rule-is-idempotent ()
  (let ((mailx-refile-rules nil)
        (rule (lambda (_) "/x")))
    (mailx-add-refile-rule rule)
    (mailx-add-refile-rule rule)
    (should (= 1 (length mailx-refile-rules)))))

(ert-deftest mailx-edge/refile-rules-alias-compatibility ()
  (let ((mailx-refile-rules nil))
    (add-to-list 'mailx--refile-rules #'ignore)
    (should (equal mailx-refile-rules '(ignore)))
    (should (equal mailx--refile-rules '(ignore)))))

(ert-deftest mailx-edge/field-display-value-variants ()
  (cl-letf (((symbol-function 'mu4e-message-field)
             (lambda (_msg field)
               (pcase field
                 (:none nil)
                 (:str "hello")
                 (:num 42)
                 (:people '((:name "Ann" :email "ann@x.org") (:email "bob@x.org")))))))
    (should (equal "(none)" (mailx--field-display-value nil :none)))
    (should (equal "hello" (mailx--field-display-value nil :str)))
    (should (equal "42" (mailx--field-display-value nil :num)))
    (should (equal "Ann, bob@x.org" (mailx--field-display-value nil :people)))))

(defun mailx-edge--unsubscribe (header)
  "Run `mailx-unsubscribe' against HEADER."
  (cl-letf (((symbol-function 'mu4e-fetch-field) (lambda (&rest _) header)))
    (mailx-unsubscribe 'msg)))

(ert-deftest mailx-edge/unsubscribe-missing-header ()
  (should-error (mailx-edge--unsubscribe nil) :type 'user-error))

(ert-deftest mailx-edge/unsubscribe-header-without-uris ()
  (should-error (mailx-edge--unsubscribe "no brackets here") :type 'user-error))

(ert-deftest mailx-edge/unsubscribe-unknown-scheme ()
  (should-error (mailx-edge--unsubscribe "<ftp://example.org/x>") :type 'user-error))

(ert-deftest mailx-edge/unsubscribe-https-browses ()
  (let (visited)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq visited u))))
      (mailx-edge--unsubscribe "<https://example.org/u>"))
    (should (equal "https://example.org/u" visited))))

(ert-deftest mailx-edge/unsubscribe-multiple-prompts ()
  (let (visited)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq visited u)))
              ((symbol-function 'completing-read) (lambda (_p uris &rest _) (cadr uris))))
      (mailx-edge--unsubscribe "<mailto:a@x.org>, <https://example.org/two>"))
    (should (equal "https://example.org/two" visited))))

(ert-deftest mailx-edge/global-mode-lifecycle-and-managed-settings ()
  (unwind-protect
      (progn
        (mailx-global-mode -1)
        (let ((mail-specify-envelope-from nil)
              (mu4e-refile-folder nil)
              (message-send-mail-function nil)
              (mml-secure-openpgp-sign-with-sender nil)
              (compose-mail-user-agent-warnings t)
              (mu4e-search-include-related t)
              (mu4e-search-results-limit 500)
              (message-fill-column 70)
              (mu4e-compose-complete-only-after nil)
              (org-capture-templates '(("t" "tasks"))))
          (should-not mailx-global-mode)
          (should-not (advice-function-member-p #'mailx--ad-guard-save (advice--symbol-function 'basic-save-buffer)))
          (should-not (member mailx--display-buffer-entry display-buffer-alist))
          (should-not (assoc "m" org-capture-templates))
          (should-not (memq #'whitespace-cleanup mu4e-compose-mode-hook))
          (mailx-global-mode 1)
          (should mailx-global-mode)
          (should (eq mail-specify-envelope-from t))
          (should (eq mu4e-refile-folder 'mailx-refile-folder))
          (should (eq message-send-mail-function 'message-send-mail-with-sendmail))
          (should (eq mml-secure-openpgp-sign-with-sender t))
          (should (null compose-mail-user-agent-warnings))
          (should (null mu4e-search-include-related))
          (should (eq mu4e-search-results-limit 1000))
          (should (eq message-fill-column 80))
          (should (stringp mu4e-compose-complete-only-after))
          (should (advice-function-member-p #'mailx--ad-guard-save (advice--symbol-function 'basic-save-buffer)))
          (should (member mailx--display-buffer-entry display-buffer-alist))
          (should (assoc "m" org-capture-templates))
          (should (memq #'whitespace-cleanup mu4e-compose-mode-hook))
          (mailx-global-mode -1)
          (should-not mailx-global-mode)
          (should (null mail-specify-envelope-from))
          (should (null mu4e-refile-folder))
          (should (null message-send-mail-function))
          (should (null mml-secure-openpgp-sign-with-sender))
          (should (eq compose-mail-user-agent-warnings t))
          (should (eq mu4e-search-include-related t))
          (should (eq mu4e-search-results-limit 500))
          (should (eq message-fill-column 70))
          (should (null mu4e-compose-complete-only-after))
          (should-not (advice-function-member-p #'mailx--ad-guard-save (advice--symbol-function 'basic-save-buffer)))
          (should-not (member mailx--display-buffer-entry display-buffer-alist))
          (should-not (assoc "m" org-capture-templates))
          (should-not (memq #'whitespace-cleanup mu4e-compose-mode-hook))))
    (mailx-global-mode 1)))

(provide 'test-mailx-edge)
;;; test-mailx-edge.el ends here
