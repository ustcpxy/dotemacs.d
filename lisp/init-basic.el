;;;;  -*- lexical-binding: t; -*-

(global-auto-revert-mode t)

(use-package smartparens
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  :config
    (sp-with-modes
        '(c++-mode objc-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package ace-pinyin
  :after avy
  :init (setq ace-pinyin-use-avy t)
  :config (ace-pinyin-global-mode t))

(setq auto-save-default nil)
(auto-save-visited-mode t)
(setq auto-save-visited-interval 1)

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude `("/ssh:"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; required >emacs28
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom
  (repeat-exit-key (kbd "RET")))

(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . #'comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  (comment-auto-fill-only-comments t))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package rime
  :custom
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir "~/.config/fcitx/rime")

  (setq rime-show-candidate 'posframe)
  )

(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  )

(provide 'init-basic)

				       
