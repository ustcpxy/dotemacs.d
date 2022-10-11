;;; init-org.el --- setup org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(defun +org-init-hacks-h ()
  "Getting org to behave."
  ;; Open file links in current window, rather than new ones
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  ;; Open directory links in dired
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-file-apps '(remote . emacs))

  ;; Open help:* links with helpful-* instead of describe-*
  (advice-add #'org-link--open-help :around #'doom-use-helpful-a)


  )

(use-package org
  :ensure nil
;  :init
;  (add-hook 'org-load-hook #'+org-init-hacks-h)
  :hook (org-load . +org-init-hacks-h)
  :config
  ;; To speed up startup, don't put to init section
  (setq org-directory "/home/derek/pkms/gtd/")
  (define-key org-mode-map (kbd "RET") '+org/dwim-at-point)

  )

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))





(provide 'init-org)
;;; init-org.el ends here
