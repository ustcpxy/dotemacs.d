;;; init-git.el -*- lexical-binding: t no-byte-compile: t -*-

(use-package magit
  :commands (magit-status)
  :config
  (add-hook 'git-commit-setup-hook
    (defun +vc-start-in-insert-state-maybe-h ()
      "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
      (when (and (bound-and-true-p evil-mode)
                 (not (evil-emacs-state-p))
                 (bobp) (eolp))
        (evil-insert-state))))
  )


(provide 'init-git)
