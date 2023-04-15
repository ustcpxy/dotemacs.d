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
  ;; (advice-add #'xref-find-definitions :around #'gtags-env-patch)
  ;; (advice-add #'xref-find-references :around #'gtags-env-patch)
  (setq xref-prompt-for-identifier nil)
)

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; 
(defun cpp-highlight-if-0/1 ()
  "Modify the face of text in between #if 0 ... #endif."
  (interactive)
    (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list '(("0" font-lock-comment-face default both)
                        ("1" default font-lock-comment-face both)))

  (cpp-highlight-buffer t))

(defun custom-c-mode-hook ()
  (cpp-highlight-if-0/1)
  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local))

;; (add-hook 'c++-mode-hook 'custom-c-mode-hook)
(add-hook 'c++-ts-mode-hook 'custom-c-mode-hook)




;;;
(use-package gxref
  :commands (gxref-xref-backend
             gxref-create-db
             gxref-update-db
             gxref-single-update-db
             gxref-set-project-dir)
  )

;; @https://emacs-china.org/t/treesit-treesit-explore-mode/23313/8
(use-package treesit
  :ensure nil
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :custom (major-mode-remap-alist
           '((c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (cmake-mode      . cmake-ts-mode)
             (conf-toml-mode  . toml-ts-mode)
             (css-mode        . css-ts-mode)
             (js-mode         . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (python-mode     . python-ts-mode)
             (sh-mode         . bash-ts-mode)
             (typescript-mode . typescript-ts-mode))))


(provide 'init-cc)
