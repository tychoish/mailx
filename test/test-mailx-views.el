;;; test-mailx-views.el --- Tests for mailx attention views -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for configurable bookmarks and tier display.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'mailx-views)

(defvar mu4e-bookmarks)
(defvar mu4e-header-info-custom)
(defvar mu4e-headers-fields)
(defvar mu4e-headers-custom-markers)

(ert-deftest mailx-views-bookmarks-are-configurable-and-unique ()
  (let* ((mailx-views-unclassified-query "maildir:/staging")
         (mailx-views-records-query "tag:record")
         (bookmarks (mailx-views-bookmarks))
         (keys (seq-map (lambda (b) (plist-get b :key)) bookmarks)))
    (should (equal (length keys) (length (delete-dups (copy-sequence keys)))))
    (should (equal (plist-get (car bookmarks) :query) "maildir:/staging"))
    (should (equal (plist-get (nth 6 bookmarks) :query) "tag:record"))
    (should (equal (plist-get (nth 1 bookmarks) :query) "tag:\"tier:reply\""))))

(ert-deftest mailx-views-tier-rejects-unknown-values ()
  (should-error (mailx-views-tier-query 'unknown) :type 'user-error))

(ert-deftest mailx-views-column-and-marker-use-tags ()
  (cl-letf (((symbol-function 'mu4e-message-field)
             (lambda (message _field) message)))
    (should (equal (mailx-views--tier-column '("receipt" "tier:cursory")) "Cursory"))
    (should (mailx-views--matches-tier-p '("tier:reply") 'reply))
    (should (equal (mailx-views--tier-column '("receipt")) ""))))

(ert-deftest mailx-views-setup-is-idempotent ()
  (let ((mu4e-bookmarks nil)
        (mu4e-header-info-custom nil)
        (mu4e-headers-fields '((:from . 22) (:subject . nil)))
        (mu4e-headers-custom-markers nil))
    (mailx-views-setup)
    (mailx-views-setup)
    (should (= 1 (length (seq-filter (lambda (x) (eq (car x) :mailx-tier))
                                     mu4e-headers-fields))))
    (should (= 1 (length mu4e-headers-custom-markers)))
    (should (assq :mailx-tier mu4e-header-info-custom))))

(ert-deftest mailx-views-jump-uses-selected-tier ()
  (let (searched)
    (cl-letf (((symbol-function 'mailx-views--unread-count) (lambda (_) 3))
              ((symbol-function 'annotated-completing-read)
               (lambda (table &rest _) (should (= (length table) 5)) "read"))
              ((symbol-function 'mu4e-search) (lambda (query) (setq searched query))))
      (mailx-jump-to-tier)
      (should (equal searched "tag:\"tier:read\"")))))

(ert-deftest mailx-views-unread-count-distinguishes-errors-from-empty ()
  (cl-letf (((symbol-function 'mailx-server-current-home) (lambda () "/tmp/db"))
            ((symbol-function 'process-file)
             (lambda (_program _in destination _display &rest _args)
               (with-temp-file (cadr destination)
                 (insert "no matches for search expression\n"))
               2)))
    (should (= 0 (mailx-views--unread-count 'reply))))
  (cl-letf (((symbol-function 'mailx-server-current-home) (lambda () "/tmp/db"))
            ((symbol-function 'process-file)
             (lambda (_program _in _destination _display &rest _args)
               (insert "no matching messages\n")
               2)))
    (should (= 0 (mailx-views--unread-count 'reply))))
  (cl-letf (((symbol-function 'mailx-server-current-home) (lambda () "/tmp/db"))
            ((symbol-function 'process-file)
             (lambda (_program _in destination _display &rest _args)
               (with-temp-file (cadr destination) (insert "database error"))
               2)))
    (should-not (mailx-views--unread-count 'reply))))

(provide 'test-mailx-views)
;;; test-mailx-views.el ends here
