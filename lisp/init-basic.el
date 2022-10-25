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

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)

  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  )

(provide 'init-basic)
