(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-elpa) ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH
(require 'init-ui)
(require 'init-evil)
(require 'init-projectile)

;; Load configs for specific features and modes

(provide 'init)
