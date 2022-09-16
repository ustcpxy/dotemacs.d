;;; init-c.el -*- lexical-binding: t no-byte-compile: t -*-

;; C/C++ Mode

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
         ("C-c c" . compile))
  :hook (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :init (setq-default c-basic-offset 4)
  :config

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'xref-backend-functions 'gxref-xref-backend)
  (advice-add #'xref-find-definitions :around #'gtags-env-patch)
  (advice-add #'xref-find-references :around #'gtags-env-patch)
  (setq xref-prompt-for-identifier nil)
)

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package gxref
  :commands (gxref-xref-backend
             gxref-create-db
             gxref-update-db
             gxref-single-update-db
             gxref-set-project-dir)
  )

(provide 'init-cc)
