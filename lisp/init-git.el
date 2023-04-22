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

  (setq magit-save-repository-buffers 'dontask)

  (defun my-magit-submodule-add-1 (origfunc url &optional path name &rest args )
    (message "url %s, path %s" url path)
    (message default-directory)
 
    (if (string-match "dotemacs" default-directory)
	(progn
	  (let* ((path (concat "site-lisp" "/" path))
		 (name (concat "site-lisp" "/" name)))
	    (funcall origfunc url path name)
	    )
	  )
      (funcall origfunc url path name)
      )
    
    )
  (advice-add 'magit-submodule-add-1 :around 'my-magit-submodule-add-1 )
  )


(provide 'init-git)
