(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

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
(require 'init-ui)
(require 'init-window)
(require 'init-basic)
(require 'init-completion)

;; Core
(require 'init-evil)

(require 'init-git)
(require 'init-org)
(require 'init-pkms)
(require 'init-cc)
;;; Keybinding Configuration
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; recentf mode
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


(require 'init-keybindings)

(provide 'init)
