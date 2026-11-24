;;; test-mailx-bulk-mu4e.el --- Tests for the mu4e apply backend -*- lexical-binding: t; -*-

;;; Commentary:
;; The mu4e server functions and mu queries are stubbed; the in-place stamp
;; helper runs against temporary files only.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mailx-bulk)

(defvar mu4e-change-filenames-when-moving)

(defconst test-mailx-bulk-mu4e--file (make-temp-file "mailx-mu4e-file-")
  "A real file standing in for a resolved message path.")

(defmacro test-mailx-bulk-mu4e--with-server (calls found &rest body)
  "Run BODY with stubbed mu4e server functions recording into CALLS.
FOUND is a function of no arguments returning the `mailx--mu-find' result."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'mailx-bulk--mu4e-active-p) (lambda (_root) t))
             ((symbol-function 'mu4e--server-move)
              (lambda (&rest args)
                (push (cons 'move (append args (list mu4e-change-filenames-when-moving)))
                      ,calls)))
             ((symbol-function 'mu4e--server-add)
              (lambda (&rest args) (push (cons 'add args) ,calls)))
             ((symbol-function 'mailx--mu-find)
              (lambda (&rest _) (funcall ,found))))
     (let ((mailx-bulk-mu4e-timeout 0.3)
           (mu4e-change-filenames-when-moving t))
       ,@body)))

(defun test-mailx-bulk-mu4e--maildir ()
  "Return a temp root with inbox/cur/m1 containing a message."
  (let* ((root (make-temp-file "mailx-mu4e-" t))
         (path (expand-file-name "inbox/cur/m1:2,S" root)))
    (make-directory (file-name-directory path) t)
    (write-region "Subject: x\nX-Label: keep\n\nbody\n" nil path nil 'silent)
    (make-directory (expand-file-name "archive/cur" root) t)
    (make-directory (expand-file-name "archive/tmp" root) t)
    root))

(ert-deftest mailx-stamp-in-place-keeps-path-and-body ()
  (let* ((root (test-mailx-bulk-mu4e--maildir))
         (path (expand-file-name "inbox/cur/m1:2,S" root)))
    (unwind-protect
        (progn
          (should (equal path (mailx-stamp-in-place path 'read '("t1"))))
          (should (equal "X-Label: keep tier:read t1\nSubject: x\n\nbody\n"
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
          (should (equal '("m1:2,S") (directory-files (file-name-directory path) nil "\\`[^.]")))
          (should-error (mailx-stamp-in-place path 'bogus nil)))
      (delete-directory root t))))

(ert-deftest mailx-stamp-in-place-refuses-symlink ()
  (let* ((root (test-mailx-bulk-mu4e--maildir))
         (path (expand-file-name "inbox/cur/m1:2,S" root))
         (link (expand-file-name "inbox/cur/l:2,S" root)))
    (unwind-protect
        (progn
          (make-symbolic-link path link)
          (should-error (mailx-stamp-in-place link 'read nil)))
      (delete-directory root t))))

(ert-deftest mailx-bulk-mu4e-active-requires-running-server-for-root ()
  (let ((root (make-temp-file "mailx-mu4e-" t)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mu4e-running-p) (lambda () t))
                    ((symbol-function 'mu4e-root-maildir) (lambda () root))
                    ((symbol-function 'mu4e--server-move) #'ignore)
                    ((symbol-function 'mu4e--server-add) #'ignore)
                    ((symbol-function 'mailx-server-current-home)
                     (lambda () (expand-file-name ".mu" root))))
            (should (mailx-bulk--mu4e-active-p root))
            (cl-letf (((symbol-function 'mailx-server-current-home)
                       (lambda () "/elsewhere/.mu")))
              (should-not (mailx-bulk--mu4e-active-p root)))
            (cl-letf (((symbol-function 'mu4e-root-maildir) (lambda () "/elsewhere")))
              (should-not (mailx-bulk--mu4e-active-p root)))
            (cl-letf (((symbol-function 'mu4e-running-p) (lambda () nil)))
              (should-not (mailx-bulk--mu4e-active-p root)))))
      (delete-directory root t))))

(ert-deftest mailx-bulk-mu4e-mark-read-moves-flags-and-confirms ()
  (let ((calls nil)
        (hitl-plan-current-target '(:root "/mail" :message-id "a@b")))
    (cl-letf (((symbol-function 'mailx-bulk--resolve-target)
               (lambda (_t) `(:path ,test-mailx-bulk-mu4e--file :maildir "/inbox" :message-id "a@b"))))
      (test-mailx-bulk-mu4e--with-server calls
          (lambda () '((:message-id "a@b" :maildir "/inbox" :flags (seen))))
        (mailx-bulk--mark-read-apply)
        (should (equal calls '((move "a@b" nil "+S-N" nil))))))))

(ert-deftest mailx-bulk-mu4e-refile-moves-without-renaming ()
  (let ((calls nil)
        (hitl-plan-current-target '(:root "/mail" :message-id "a@b")))
    (cl-letf (((symbol-function 'mailx-bulk--resolve-target)
               (lambda (_t) `(:path ,test-mailx-bulk-mu4e--file :maildir "/inbox" :message-id "a@b"))))
      (test-mailx-bulk-mu4e--with-server calls
          (lambda () '((:message-id "a@b" :maildir "/archive")))
        (mailx-bulk--refile-apply "/archive")
        (should (equal calls '((move "a@b" "/archive" nil t nil)))))
      (setq calls nil)
      (test-mailx-bulk-mu4e--with-server calls (lambda () nil)
        (should-error (mailx-bulk--refile-apply "/../x") :type 'user-error)
        (should-not calls)))))

(ert-deftest mailx-bulk-mu4e-refile-times-out-when-database-never-updates ()
  (let ((calls nil)
        (hitl-plan-current-target '(:root "/mail" :message-id "a@b")))
    (cl-letf (((symbol-function 'mailx-bulk--resolve-target)
               (lambda (_t) `(:path ,test-mailx-bulk-mu4e--file :maildir "/inbox" :message-id "a@b"))))
      (test-mailx-bulk-mu4e--with-server calls
          (lambda () '((:message-id "a@b" :maildir "/inbox")))
        (should-error (mailx-bulk--refile-apply "/archive"))
        (should (= 1 (length calls)))))))

(ert-deftest mailx-bulk-mu4e-classify-stamps-adds-then-moves-in-order ()
  (let* ((root (test-mailx-bulk-mu4e--maildir))
         (path (expand-file-name "inbox/cur/m1:2,S" root))
         (calls nil)
         (polls 0)
         (hitl-plan-current-target (list :root root :message-id "a@b")))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx-bulk--resolve-target)
                   (lambda (_t) (list :path path :maildir "/inbox" :message-id "a@b")))
                  ((symbol-function 'mailx-stamp-and-move)
                   (lambda (&rest _) (error "filesystem path must not run"))))
          (test-mailx-bulk-mu4e--with-server calls
              (lambda ()
                (setq polls (1+ polls))
                (if (< polls 3)
                    '((:message-id "a@b" :maildir "/inbox" :tags ("keep")))
                  '((:message-id "a@b" :maildir "/archive" :tags ("keep" "tier:read" "t1")))))
            (mailx-bulk--classify-apply 'read '("t1") "/archive")
            (should (equal (reverse calls)
                           (list (list 'add path)
                                 (list 'move "a@b" "/archive" nil t nil))))
            (should (>= polls 3))
            (should (string-match-p "X-Label: keep tier:read t1"
                                    (with-temp-buffer
                                      (insert-file-contents path)
                                      (buffer-string))))))
      (delete-directory root t))))

(ert-deftest mailx-bulk-mu4e-classify-same-folder-skips-move ()
  (let* ((root (test-mailx-bulk-mu4e--maildir))
         (path (expand-file-name "inbox/cur/m1:2,S" root))
         (calls nil)
         (hitl-plan-current-target (list :root root :message-id "a@b")))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx-bulk--resolve-target)
                   (lambda (_t) (list :path path :maildir "/inbox" :message-id "a@b"))))
          (test-mailx-bulk-mu4e--with-server calls
              (lambda () '((:message-id "a@b" :maildir "/inbox" :tags ("tier:cursory"))))
            (mailx-bulk--tag-apply 'cursory nil)
            (should (equal calls (list (list 'add path))))))
      (delete-directory root t))))

(ert-deftest mailx-bulk-mu4e-errors-when-mu4e-too-old ()
  ;; Unbind the server functions so a mu4e autoload cannot start a server.
  (cl-letf (((symbol-function 'mailx-bulk--mu4e-active-p) (lambda (_r) t))
            ((symbol-function 'mu4e--server-move) nil)
            ((symbol-function 'mu4e--server-add) nil)
            ((symbol-function 'mailx-bulk--resolve-target)
             (lambda (_t) `(:path ,test-mailx-bulk-mu4e--file :maildir "/inbox" :message-id "a@b"))))
    (let ((hitl-plan-current-target '(:root "/m" :message-id "a@b")))
      (should-error (mailx-bulk--mark-read-apply)))))

(ert-deftest mailx-bulk-falls-back-to-filesystem-when-mu4e-inactive ()
  (let* ((root (test-mailx-bulk-mu4e--maildir))
         (path (expand-file-name "inbox/cur/m1:2,S" root))
         (hitl-plan-current-target (list :root root :message-id "a@b")))
    (unwind-protect
        (cl-letf (((symbol-function 'mailx-bulk--mu4e-active-p) (lambda (_r) nil))
                  ((symbol-function 'mailx-bulk--resolve-target)
                   (lambda (_t) (list :path path :maildir "/inbox"))))
          (mailx-bulk--classify-apply 'read '("t1") "/archive")
          (should (file-exists-p (expand-file-name "archive/cur/m1:2,S" root))))
      (delete-directory root t))))

(defun test-mailx-bulk-mu4e--cleanup ()
  "Delete the stand-in message file."
  (when (file-exists-p test-mailx-bulk-mu4e--file)
    (delete-file test-mailx-bulk-mu4e--file)))

(add-hook 'kill-emacs-hook #'test-mailx-bulk-mu4e--cleanup)

(provide 'test-mailx-bulk-mu4e)
;;; test-mailx-bulk-mu4e.el ends here
