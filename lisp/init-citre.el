;;; init-citre.el -*- lexical-binding: t no-byte-compile: t -*-

(require 'citre)
(require 'citre-config)

;; @https://emacs-china.org/t/citre-ctags/17604/534
(defun filter-imenu (imenu-alist)
  (seq-filter (lambda (item) (member (car item) '("function" "class")))
              imenu-alist))
(advice-add 'citre-imenu-create-index-function :filter-return #'filter-imenu)

;; @https://github.com/universal-ctags/citre/wiki/Useful-commands
(defun citre-jump+ ()
  (interactive)
  (condition-case _
      (citre-jump)
    (error (let* ((xref-prompt-for-identifier nil))
             (call-interactively #'xref-find-definitions)))))

(defun my--push-point-to-xref-marker-stack (&rest r)
  (xref-push-marker-stack (point-marker)))
(dolist (func '(find-function
                counsel-imenu
                helm-imenu
                projectile-grep
                helm-grep-ag
                counsel-rg
                lsp-ivy-workspace-symbol
                citre-jump))
  (advice-add func :before 'my--push-point-to-xref-marker-stack))

(provide 'init-citre)
