;; -*- coding: utf-8; lexical-binding: t; -*-

(let* ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required" minver)))

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *emacs28* (>= emacs-major-version 28))

;; Light weight mode, fewer packages are used.
(setq my-lightweight-mode-p (and (boundp 'startup-now) (eq startup-now t)))
(defun add-subdirs-to-load-path (dir)

  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-package)

;;; keep ~.emacs.d~ folders clean
(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(require 'init-funcs)
(require 'init-keyfreq)
(require 'init-ui)
(require 'init-window)
(require 'init-basic)
(require 'init-completion)

;; Core
;; (require 'init-evil)

(require 'init-matchit)
(require 'init-git)
(require 'init-org)
(require 'init-pkms)
(require 'init-cc)
(require 'init-treesit)
(require 'init-citre)
(require 'init-thing-edit)
(require 'init-kill-ring)
(require 'init-blink-search)
(require 'init-symbol-overlay)

;;; Keybinding Configuration

(require 'init-keybindings)

(provide 'init)
