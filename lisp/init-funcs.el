;;; init-funcs.el --- -*- lexical-binding: t; -*-

(defun my/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun my/evil-quick-replace (beg end)
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun my/imenu ()
  (interactive)
  (if (eq major-mode #'org-mode)
      (call-interactively #'consult-org-heading)
    (call-interactively #'consult-imenu)))

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (startup--get-buffer-create-scratch)))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window.
If `spacemacs-layouts-restrict-spc-tab' is `t' then this only switches between
the current layouts buffers."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p spacemacs-layouts-restrict-spc-tab)
          (let ((buffer-list (persp-buffer-list))
                (my-buffer (window-buffer window)))
            ;; find buffer of the same persp in window
            (seq-find (lambda (it) ;; predicate
                        (and (not (eq (car it) my-buffer))
                             (member (car it) buffer-list)))
                      (window-prev-buffers)
                      ;; default if found none
                      (list nil nil nil)))
        (or (cl-find (window-buffer window) (window-prev-buffers)
                     :key #'car :test-not #'eq)
            (list (other-buffer) nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

;; https://posts.michaelks.org/automatic-gtags-integration-emacs-git/
(defun gtags-env-patch (orig &rest args)
  (message "test")
  (if-let* ((project-root (file-truename (locate-dominating-file "." ".git")))
            (git-dir (expand-file-name ".git" project-root))
            (process-environment (append
                                  (list (format "GTAGSROOT=%s" project-root)
                                        (format "GTAGSDBPATH=%s" git-dir))
                                  process-environment)))
      (apply orig args)
    (apply orig args)))

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

(provide 'init-funcs)
