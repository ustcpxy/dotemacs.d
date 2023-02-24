;;; init-citre.el -*- lexical-binding: t no-byte-compile: t -*-

(require 'citre)
(require 'citre-config)

;; @https://emacs-china.org/t/citre-ctags/17604/534
(defun filter-imenu (imenu-alist)
  (seq-filter (lambda (item) (member (car item) '("function" "class")))
              imenu-alist))
(advice-add 'citre-imenu-create-index-function :filter-return #'filter-imenu)

(require 'color-rg)
;; @https://github.com/manateelazycat/color-rg/issues/58
(add-hook 'color-rg-mode-hook 'evil-insert-state)

(provide 'init-citre)
