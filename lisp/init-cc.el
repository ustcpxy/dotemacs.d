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

(require 'fingertip)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               'typescript-mode-hook
               
               'c-ts-mode-hook
               'c++-ts-mode-hook
               'cmake-ts-mode-hook
               'toml-ts-mode-hook
               'css-ts-mode-hook
               'js-ts-mode-hook
               'json-ts-mode-hook
               'python-ts-mode-hook
               'bash-ts-mode-hook
               'typescript-ts-mode-hook
               ))
  (add-hook hook '(lambda () (fingertip-mode 1))))
(define-key fingertip-mode-map (kbd "(") 'fingertip-open-round)
(define-key fingertip-mode-map (kbd "[") 'fingertip-open-bracket)
(define-key fingertip-mode-map (kbd "{") 'fingertip-open-curly)
(define-key fingertip-mode-map (kbd ")") 'fingertip-close-round)
(define-key fingertip-mode-map (kbd "]") 'fingertip-close-bracket)
(define-key fingertip-mode-map (kbd "}") 'fingertip-close-curly)
(define-key fingertip-mode-map (kbd "=") 'fingertip-equal)

(define-key fingertip-mode-map (kbd "%") 'fingertip-match-paren)
(define-key fingertip-mode-map (kbd "\"") 'fingertip-double-quote)
(define-key fingertip-mode-map (kbd "'") 'fingertip-single-quote)

(define-key fingertip-mode-map (kbd "SPC") 'fingertip-space)
(define-key fingertip-mode-map (kbd "RET") 'fingertip-newline)

(define-key fingertip-mode-map (kbd "M-o") 'fingertip-backward-delete)
(define-key fingertip-mode-map (kbd "C-d") 'fingertip-forward-delete)
(define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

(define-key fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
(define-key fingertip-mode-map (kbd "M-'") 'fingertip-wrap-single-quote)
(define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
(define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
(define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
(define-key fingertip-mode-map (kbd "M-)") 'fingertip-unwrap)

(define-key fingertip-mode-map (kbd "M-p") 'fingertip-jump-right)
(define-key fingertip-mode-map (kbd "M-n") 'fingertip-jump-left)
(define-key fingertip-mode-map (kbd "M-:") 'fingertip-jump-out-pair-and-newline)

(define-key fingertip-mode-map (kbd "C-j") 'fingertip-jump-up)
(provide 'init-cc)
